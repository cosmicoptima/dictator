{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Items
    ( Items(..)
    , parseItems
    , parseTrade
    , parseWords
    , parseCredit
    , parseTrinkets
    , parseTrinketPair
    , pprint
    , itemCredits
    -- , itemUsers
    , itemTrinkets
    -- , itemWords
    ) where

import qualified Prelude
import           Relude                  hiding ( (<|>)
                                                , many
                                                , optional
                                                )
import           Relude.Unsafe                  ( read )

import           Datatypes

import           Control.Lens
import           Data.Default
import qualified Data.Text                     as T
import           Discord.Types
import           Text.Parsec
import           Text.Parsec.Text               ( Parser )


pprint :: [ItemSyntax] -> Text
pprint []  = "nothing"
pprint its = T.intercalate ", " $ fmap show its

-- | Utility function that ensures a given parser matches no more
andEof :: Parser a -> Parser a
andEof par = do
    r <- par
    eof
    return r

parSep :: Parser ()
parSep = (void . string $ " and ") <|> do
    void $ char ','
    optional space

type WordItem = Text
type UserItem = UserId

-- | Represents the possible items that can be parsed. Not meant to be used by library consumers!
data ItemSyntax
    = WordItem WordItem
    | UserItem UserItem
    | CreditItem Credit
    | TrinketItem TrinketID
    deriving (Eq)

instance Prelude.Show ItemSyntax where
    show (UserItem    what) = "@" ++ show what
    show (WordItem    what) = show what
    show (CreditItem  c   ) = show c ++ "c"
    show (TrinketItem item) = "#" ++ show item

-- | Parse a user by mention.
parUserItem :: Parser UserItem
parUserItem = do
    uId <- between couldMention (string ">") (many1 digit)
    return $ read uId
    where couldMention = try (string "<!@") <|> string "<"

-- | Parse a word in quotes.
parWordItem :: Parser WordItem
parWordItem = do
    word <- between (char '"') (char '"') (many1 lower)
    return $ fromString word

-- | Parse some credits as a float.
parCreditItem :: Parser Credit
parCreditItem = do
    sign  <- option "" $ string "-"
    fHead <- many1 digit
    fTail <- option ".0" $ do
        pt    <- char '.'
        fTail <- many1 digit
        return $ pt : fTail
    void $ char 'c'
    return . read $ sign <> fHead <> fTail

parTrinketItem :: Parser TrinketID
parTrinketItem = do
    void $ char '#'
    res <- some digit
    return $ read res

parTrinketPair :: Parser (TrinketID, TrinketID)
parTrinketPair = do
    item1 <- parTrinketItem
    void $ string " and "
    item2 <- parTrinketItem
    return (item1, item2)

-- | Parse any unique item, with one case for each constructor of Item.
parItem :: Parser ItemSyntax
parItem =
    (parUserItem <&> UserItem)
        <|> (parWordItem <&> WordItem)
        <|> (parCreditItem <&> CreditItem)
        <|> (parTrinketItem <&> TrinketItem)
        <?> "an item (options are: @user, \"word\", #trinket, 1c)"


-- | Parse comma-seperated items or nothing.
parItems :: Parser [ItemSyntax]
parItems = try parNothing <|> sepBy1 parItem parSep
  where
    parNothing = do
        void $ string "nothing"
        return []


data Items = Items
    { _itemCredits  :: Credit
    -- , _itemWords    :: [WordItem]
    -- , _itemUsers    :: [UserItem]
    , _itemTrinkets :: [TrinketID]
    }
    deriving Eq

-- makeLensesFor (fmap (\w -> (w, w <> "L")) ["itemCredits", "itemWords", "itemUsers", "itemTrinkets"]) ''Items
makeLenses ''Items

instance Prelude.Show Items where
    show its = if val == "0.0c" then "nothing" else val
      where
        val = show' its
        show' it = intercalate
            ", "
            ( (show (it ^. itemCredits) ++ "c")
            : fmap (("#" ++) . show) (it ^. itemTrinkets)
            )
                -- ++ showWords (it ^. itemWords)
                -- ++ showUsers (it ^. itemUsers)
        -- showWords = map $ show . WordItem
        -- showUsers = map $ show . UserItem

instance Default Items where
    def = Items 0 []

-- | Parse a two-sided trade.
parTrade :: Parser ([ItemSyntax], [ItemSyntax])
parTrade = do
    offers        <- parItems
    -- We should be able to omit this bit!
    -- Specifically, omitting demands should be seen as an act of charity. :)
    seconDictMalf <- optionMaybe delim
    demands       <- case seconDictMalf of
        Just _  -> parItems
        Nothing -> return []
    return (offers, demands)
    where delim = between space space $ string "for" <|> string "demanding"

collateItems :: [ItemSyntax] -> Items
collateItems = foldr includeItem def  where
    includeItem (CreditItem  c) st = st & itemCredits +~ c
    includeItem (WordItem    _) st = st
    includeItem (UserItem    _) st = st
    includeItem (TrinketItem t) st = st & itemTrinkets %~ (t :)

parseWords :: Text -> Either ParseError [WordItem]
parseWords = parse (andEof $ sepBy1 parWordItem parSep) ""

parseTrinkets :: Text -> Either ParseError [TrinketID]
parseTrinkets =
    parse (sepBy1 parTrinketItem parSep <* eof) "" . fromString . toString

parseTrinketPair :: Text -> Either ParseError (TrinketID, TrinketID)
parseTrinketPair = parse (parTrinketPair <* eof) ""

parseCredit :: Text -> Either ParseError Credit
parseCredit = parse (andEof parCreditItem) ""

parseItems :: Text -> Either ParseError Items
parseItems txt = case parse (andEof parItems) "" txt of
    Left  err -> Left err
    Right its -> Right $ collateItems its


parseTrade :: Text -> Either ParseError (Items, Items)
parseTrade txt = case parse (andEof parTrade) "" txt of
    Left err -> Left err
    Right (offering, demanding) ->
        Right (collateItems offering, collateItems demanding)
