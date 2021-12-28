{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Economy
    ( mkNewTrinket
    , getNewTrinket
    , getRandomTrinket
    , userOwns
    , punishWallet
    , combineTrinkets
    , itemsToUser
    , userToItems
    , takeItems
    , takeOrPunish
    , fromTrinkets
    , fromCredits
    , printTrinkets
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Datatypes
import           DiscordUtils
import           GenText                        ( J1Opts(J1Opts)
                                                , getJ1
                                                , getJ1With
                                                , makePrompt
                                                )
import           Items

import           Data.MultiSet                  ( MultiSet )

import           Control.Lens
import           Data.Char
import           Data.Default                   ( def )
import           Data.List                      ( maximum )
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Discord.Internal.Types.Prelude
import           System.Random
import           Text.Parsec                    ( ParseError
                                                , anyChar
                                                , eof
                                                , manyTill
                                                , parse
                                                , string
                                                )

commonTrinketExamples :: [Text]
commonTrinketExamples =
    [ "3.67oz of rust"
    , "a small bird"
    , "a new mobile phone"
    , "a ball of purple yawn"
    , "two message board roles"
    , "silly little thing"
    , "an oily tin can"
    ]

rareTrinketExamples :: [Text]
rareTrinketExamples =
    [ "a ball of pure malignant evil"
    , "the awfulness of your post"
    , "nothing"
    , "a gateway into another world"
    , "a bag of dicks"
    , "the ability to control time"
    , "a machete"
    , "an empty warehouse"
    , "a free pass to ban one member"
    ]

validTrinketName :: Text -> Bool
validTrinketName t =
    (isLower . T.head) t'
        &&        t'
        `notElem` (commonTrinketExamples <> rareTrinketExamples)
    where t' = T.strip t

getRandomTrinket :: DB.Connection -> DictM (TrinketID, TrinketData)
getRandomTrinket conn = do
    maxTrinketID     <- nextTrinketId conn <&> pred
    trinketID        <- randomRIO (1, maxTrinketID)
    Just trinketData <- getTrinket conn trinketID
    return (trinketID, trinketData)

-- | not only retrieves a new trinket, but adds it to the database
mkNewTrinket :: DB.Connection -> Rarity -> DictM (TrinketID, TrinketData)
mkNewTrinket conn rarity = do
    tId     <- nextTrinketId conn
    trinket <- getNewTrinket conn rarity
    setTrinket conn tId trinket
    return (tId, trinket)

getNewTrinket :: DB.Connection -> Rarity -> DictM TrinketData
getNewTrinket conn rarity = do
    let rare = rarity == Rare
    res <- getJ1 16 (prompt rare)
    case listToMaybe . rights . fmap parseTrinketName . lines $ res of
        Just name -> if validTrinketName name
            then return $ TrinketData name rarity
            else getNewTrinket conn rarity
        Nothing -> getNewTrinket conn rarity
  where
    promptTrinkets rare = makePrompt . map (<> ".") $ if rare
        then rareTrinketExamples
        else commonTrinketExamples
    prompt rare =
        "There exists a dictator of an online chatroom who is eccentric but evil. He often gives out items. Here are some examples of "
            <> itemDesc
            <> " items.\n"
            <> promptTrinkets rare
        where itemDesc = if rare then "rare" else "common"

combineTrinkets
    :: DB.Connection
    -> TrinketData
    -> TrinketData
    -> DictM (TrinketID, TrinketData)
combineTrinkets conn t1 t2 = do
    res <- getJ1With (J1Opts 0.8 0.9) 16 prompt
    let rarity = maximum . map (view trinketRarity) $ [t1, t2]
    let mayTrinket =
            join
                . fmap (rightToMaybe . parseTrinketCombination)
                . listToMaybe
                . lines
                $ res
    case mayTrinket of
        Nothing   -> combineTrinkets conn t1 t2
        Just name -> do
            let trinket = TrinketData name rarity
            tId <- nextTrinketId conn
            setTrinket conn tId trinket
            return (tId, trinket)
  where
    examples =
        [ "In an online message board, items can be combined together to create new items. Here are some examples of various combinations."
        , "Item 1: a tin can. Item 2: 3.2g of gunpowder. Result: a bomb."
        , "Item 1: a small bird. Item 2: a small bird. Result: a large bird."
        , "Item 1: a permit to ban one user of your choice. Item 2: another user. Result: BANNED."
        , "Item 1: a remote tropical island with coconuts and a treasure chest and rum. Item 2: a rusty key. Result: rum"
        , "Item 1: a bag of dicks. Item 2: your ass. Result: dicks up your ass."
        , "Item 1: Just a sandy sea. Item 2: A spare rubber x. Result: gibberish."
        , "Item 1: an ache of discontent. Item 2: a polaroid peel-off. Result: a depressing photograph."
        , "Item 1: a tiny cookie. Item 2: blood dreams of a dead end Result: a cookie with blood."
        , "Item 1: a baby with no arms or legs. Item 2: arms and legs. Result: a baby."
        , "Item 1: the ability to control time. Item 2: a portal to another dimension. Result: Godhood."
        , "Item 1: a poor girl. Item 2: a heart. Result: a happy girl."
        ]
    prompt =
        unlines examples
            <> "\nItem 1: "
            <> t1
            ^. trinketName
            <> ". Item 2: "
            <> t2
            ^. trinketName
            <> ". Result: "

parseTrinketName :: Text -> Either ParseError Text
parseTrinketName = parse
    (fmap fromString $ string "- " *> manyTill anyChar (string ".") <* eof)
    ""

parseTrinketCombination :: Text -> Either ParseError Text
parseTrinketCombination =
    parse (fromString <$> manyTill anyChar (string ".")) ""

nextTrinketId :: DB.Connection -> DictM Int
nextTrinketId conn = go 1  where
    go n = do
        trinket <- getTrinket conn n
        case trinket of
            Just _  -> go (n + 1)
            Nothing -> return n

printTrinkets :: DB.Connection -> MultiSet TrinketID -> DictM [Text]
printTrinkets conn trinkets = do
    pairs <-
        (forM (MS.elems trinkets) $ \t -> do
                trinketData <- getTrinket conn t
                return (t, trinketData)
            )
            <&> MS.fromList
    let displays = MS.map
            (\case
                (trinket, Just trinketData) ->
                    Just $ displayTrinket trinket trinketData
                (_, Nothing) -> Nothing
            )
            pairs
    (return . MS.elems . MS.mapMaybe id) displays

fromTrinkets :: MultiSet TrinketID -> Items
fromTrinkets trinkets = def & itemTrinkets .~ trinkets

fromCredits :: Credit -> Items
fromCredits credits = def & itemCredits .~ credits

-- | Project a user into a collection of only their item data.
userToItems :: UserData -> Items
userToItems userData = Items { _itemCredits  = userData ^. userCredits
                             , _itemTrinkets = userData ^. userTrinkets
                             }

-- | Given a user, update their item data to the following.
itemsToUser :: UserData -> Items -> UserData
itemsToUser userData items =
    userData
        &  userCredits
        .~ (items ^. itemCredits)
        &  userTrinkets
        .~ (items ^. itemTrinkets)

-- | Take a set of items from a user.
takeItems :: DB.Connection -> UserId -> Items -> DictM ()
takeItems conn user items = void $ modifyUser conn user removeItems
  where
    removeItems userData =
        userData
            &  userCredits
            -~ (items ^. itemCredits)
            &  userTrinkets
            %~ (MS.\\ (items ^. itemTrinkets))

userOwns :: UserData -> Items -> Bool
userOwns userData items =
    let
        Items { _itemCredits = claimedCredits, _itemTrinkets = claimedTrinkets }
            = items
        ownsCredits =
            claimedCredits <= 0 || (userData ^. userCredits) >= claimedCredits
        ownsTrinkets =
            claimedTrinkets `MS.isSubsetOf` (userData ^. userTrinkets)
    in
        ownsCredits && ownsTrinkets

punishWallet :: DB.Connection -> UserId -> DictM ()
punishWallet conn user = do
    sendMessageToGeneral
        "You don't have the goods you so shamelessly claim ownership to, and now you own even less. Credits, that is."
    void $ modifyUser conn user (over userCredits pred)

takeOrPunish :: DB.Connection -> UserId -> Items -> DictM Bool
takeOrPunish conn user items = do
    userData <- getUser conn user <&> fromMaybe def
    let res = userOwns userData items
    if res then takeItems conn user items else punishWallet conn user
    return res
