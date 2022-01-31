-- | Defines parsers and types for items.

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Items
    ( Items(..)
    , addItems
    , parseItems
    , parseTrade
    , parseWords
    , parseCredit
    , parTrinketItem
    , parseTrinkets
    , parseTrinketPair
    , pprint
    , itemCredits
    , itemUsers
    , itemTrinkets
    , itemWords
    , parseTrinketsAndLocations
    , parseUserAndName
    , TrinketID
    , Credit
    ) where

import qualified Prelude
import           Relude                  hiding ( (<|>)
                                                , many
                                                , optional
                                                )
import           Relude.Unsafe                  ( read )

import qualified Data.MultiSet                 as MS
import           Data.MultiSet                  ( MultiSet )

import           Control.Lens
import           Data.Default
import qualified Data.Text                     as T
import           Discord.Types
import           Text.Parsec
import           Text.Parsec.Text               ( Parser )


type TrinketID = Int
type Credit = Double

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
    show (UserItem    what) = "<@!" ++ show what ++ ">"
    show (WordItem    what) = show what
    show (CreditItem  c   ) = show c ++ "c"
    show (TrinketItem item) = "#" ++ show item

-- | Parse a user by mention.
parUserItem :: Parser UserItem
parUserItem = do
    uId <- between couldMention (string ">") (many1 digit)
    return $ read uId
    where couldMention = try (string "<@!") <|> string "<@"

-- | Parse a word in quotes.
parWordItem :: Parser WordItem
parWordItem = do
    word <- between (char '"') (char '"') (many1 lower)
    return $ fromString word

-- | Parse some credits as a float.
parCreditItem :: Parser Credit
parCreditItem = do
    fHead <- many1 digit
    fTail <- option ".0" $ do
        pt    <- char '.'
        fTail <- many1 digit
        return $ pt : fTail
    void $ string "c" <|> string " credits"
    return . read $ fHead <> fTail

parTrinketItem :: Parser TrinketID
parTrinketItem = do
    void $ char '#'
    res <- some digit
    return $ read res

parTrinketPair :: Parser (TrinketID, TrinketID)
parTrinketPair = do
    item1 <- parTrinketItem
    try parSep <|> void space
    item2 <- parTrinketItem
    return (item1, item2)

parTrinketsAndLocation :: Parser (MultiSet TrinketID, Text)
parTrinketsAndLocation = do
    trinkets <- sepBy1 parTrinketItem (try parSep)
    void $ string " in "
    location <- many1 anyChar
    return (MS.fromList trinkets, fromString location)

parUserAndName :: Parser (UserItem, [WordItem])
parUserAndName = do
    user <- parUserItem
    void $ many1 space
    name <- sepBy1 parWordItem (try parSep)
    return (user, name)

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
parItems = try parNothing <|> sepBy1 parItem (try parSep)
  where
    parNothing = do
        void $ string "nothing"
        return []


data Items = Items
    { _itemCredits  :: Credit
    , _itemWords    :: MultiSet WordItem
    , _itemUsers    :: MultiSet UserItem
    , _itemTrinkets :: MultiSet TrinketID
    }
    deriving (Eq, Generic, Show, Read)

-- makeLensesFor (fmap (\w -> (w, w <> "L")) ["itemCredits", "itemWords", "itemUsers", "itemTrinkets"]) ''Items
makeLenses ''Items

-- instance Default (MS.MultiSet a) where
--     def = MS.empty

instance Default Items where
    def = Items { _itemCredits  = 0
                , _itemWords    = MS.empty
                , _itemTrinkets = MS.empty
                , _itemUsers    = MS.empty
                }

addItems :: Items -> Items -> Items
addItems it1 it2 =
    let
        Items { _itemCredits = c1, _itemWords = w1, _itemTrinkets = t1, _itemUsers = u1 }
            = it1
        Items { _itemCredits = c2, _itemWords = w2, _itemTrinkets = t2, _itemUsers = u2 }
            = it2
    in
        Items { _itemCredits  = c1 + c2
              , _itemWords    = w1 <> w2
              , _itemTrinkets = t1 <> t2
              , _itemUsers    = u1 <> u2
              }

-- | Parse a two-sided trade.
parTrade :: Parser ([ItemSyntax], [ItemSyntax])
parTrade = try parDemands <|> parNoDemands
  where
    parNoDemands = do
        offers <- parItems
        return (offers, [])

    parDemands = do
        offers <- parItems
        void $ try (string " for ") <|> string " demanding "
        demands <- parItems
        return (offers, demands)

collateItems :: [ItemSyntax] -> Items
collateItems = foldr includeItem def  where
    includeItem (CreditItem  c) st = st & itemCredits +~ c
    includeItem (WordItem    w) st = st & itemWords %~ MS.insert w
    includeItem (UserItem    u) st = st & itemUsers %~ MS.insert u
    includeItem (TrinketItem t) st = st & itemTrinkets %~ MS.insert t

parseWords :: Text -> Either ParseError [Text]
parseWords = parse (andEof $ sepBy1 parWordItem parSep) ""

parseTrinkets :: Text -> Either ParseError (MultiSet TrinketID)
parseTrinkets =
    fmap MS.fromList
        . parse (sepBy1 parTrinketItem parSep <* eof) ""
        . fromString
        . toString

parseTrinketsAndLocations
    :: Text -> Either ParseError (MultiSet TrinketID, Text)
parseTrinketsAndLocations = parse parTrinketsAndLocation ""

parseUserAndName :: Text -> Either ParseError (UserItem, [WordItem])
parseUserAndName = parse parUserAndName ""

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
