{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- We don't need to use any of these results in parsing, mostly
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

module Items
    ( Item
    , parseItems
    , parseDeal
    , pprint
    ) where

import qualified Data.Text                     as T
-- import           Discord
-- import           Discord.Internal.Rest.Prelude  ( Request )
-- import           Discord.Requests
import           Discord.Types
import qualified Prelude
import           Relude                  hiding ( (<|>)
                                                , many
                                                , optional
                                                )
import           Relude.Unsafe                  ( read )
import           Text.Parsec
import           Text.Parsec.Combinator
import           Text.Parsec.Text               ( Parser )

pprint :: [Item] -> Text
pprint []  = "nothing"
pprint its = T.intercalate ", " $ fmap show its

-- | Utility function that ensures a given parser matches no more
andEof :: Parser a -> Parser a
andEof par = do
    r <- par
    eof
    return r

type WordItem = Text
type UserItem = UserId

-- | Datatype for unique, non-fungible items. Do not under any circumstances attempt to include credits!
data Item
    = WordItem WordItem
    | UserItem UserItem
    deriving (Eq)

instance Show Item where
    show (UserItem what) = "@" <> show what
    show (WordItem what) = show what

-- | Parse a user by mention.
parUserItem :: Parser Item
parUserItem = do
    uId <- between couldMention (string ">") (many1 digit)
    return (UserItem $ read uId)
    where couldMention = try (string "<!@") <|> string "<"

-- | Parse a word in quotes.
parWordItem :: Parser Item
parWordItem = do
    word <- between (char '"') (char '"') (many1 lower)
    return (WordItem $ fromString word)

-- | Parse any unique item, with one case for each constructor of Item.
parItem :: Parser Item
parItem =
    parUserItem <|> parWordItem <?> "an item (options are: @user, \"word\")"

-- | Parse comma-seperated items or nothing.
parItems :: Parser [Item]
parItems = try parNothing <|> sepBy1 parItem parSep
  where
    parNothing = do
        string "nothing"
        return []
    parSep = do
        char ','
        optional space

-- | Parse a two-sided trade.
parTrade :: Parser ([Item], [Item])
parTrade = do
    offers     <- parItems
    -- We should be able to omit this bit!
    -- Specifically, omitting demands should be seen as an act of charity. :)
    secondHalf <- optionMaybe delim
    demands    <- case secondHalf of
        Just _  -> parItems
        Nothing -> return []
    return (offers, demands)
    where delim = between space space $ string "for" <|> string "demanding"

-- | Should be used when nothing is up for offer, as in the previous recombobulate command.
parseItems :: Text -> Either ParseError [Item]
parseItems = parse (andEof parItems) ""

-- | Should be used for two-sided trades, as in the previous offer command.
parseDeal :: Text -> Either ParseError ([Item], [Item])
parseDeal = parse (andEof parTrade) ""
