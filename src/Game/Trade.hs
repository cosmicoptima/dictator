{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Game.Trade where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Game.Data
import           Utils.DictM
import           Utils.Discord

import           Discord.Requests
import           Discord.Types

import           Control.Lens
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.Random           ( randomRIO )
import qualified Database.Redis                as DB
import           Game                           ( decrementWallet
                                                , fromCredits
                                                , fromTrinket
                                                , giveItems
                                                , ownsOrComplain
                                                , takeItems
                                                , userOwns
                                                )
import           Game.Events                    ( randomTrinket )

tradeDesc :: TradeStatus -> Text
tradeDesc OpenTrade   = "Offer (OPEN: react with 🤝 to accept)"
-- tradeDesc PendingTrade = "Offer (PENDING)"
tradeDesc ClosedTrade = "Offer (CLOSED)"

tradeColour :: TradeStatus -> ColorInteger
tradeColour OpenTrade = 0x2ecc71
tradeColour _         = 0x888888

makeOfferEmbed :: DB.Connection -> TradeData -> DictM CreateEmbed
makeOfferEmbed conn tradeData = do
    let TradeData status offers demands offerer = tradeData
    showOffers  <- displayItems conn offers
    showDemands <- displayItems conn demands
    let offersDesc  = ("Offers", showOffers)
        demandsDesc = ("Demands", showDemands)
        descDesc    = "Offered by <@" <> show offerer <> ">"
        titleDesc   = tradeDesc status
        colour      = tradeColour status
    return $ mkEmbed titleDesc descDesc [demandsDesc, offersDesc] (Just colour)

handleTrade
    :: DB.Connection
    -> ChannelId
    -> MessageId
    -> TradeData
    -> UserId
    -> DictM ()
handleTrade conn channel message tradeData buyer = do
    let TradeData status offers demands seller = tradeData
    when (status == OpenTrade) $ if buyer == seller
        then do
            sendMessage
                channel
                "Do you believe I can't tell humans apart? You can't accept your own offer. It has been cancelled instead."
            cancelTrade conn channel message tradeData
        else do
            ownsOrComplain conn buyer demands
            -- Manual error handling and ownership checks because trades are delayed.
            offersOwned <- flip userOwns offers <$> getUserOr Fuckup conn seller
            if not offersOwned
                then do
                    let mention = "<@" <> show seller <> ">"
                    sendMessage channel
                        $ "You don't have the goods you've put up for offer, "
                        <> mention
                        <> ". Your trade has been cancelled and your credits have been decremented."
                    decrementWallet conn seller
                else do
                    takeItems conn seller offers
                    giveItems conn buyer offers
                    takeItems conn buyer demands
                    giveItems conn seller demands
                    sendMessage channel
                        $  "Transaction successful. Congratulations, <@"
                        <> show buyer
                        <> ">."
            cancelTrade conn channel message tradeData

cancelTrade :: DB.Connection -> ChannelId -> MessageId -> TradeData -> DictM ()
cancelTrade conn channel message trade = do
    let closedTrade = trade & tradeStatus .~ ClosedTrade
    embed <- makeOfferEmbed conn closedTrade
    setTrade conn message closedTrade
    restCall'_ $ EditMessage (channel, message) "" (Just embed)

openTrade :: DB.Connection -> ChannelId -> TradeData -> DictM MessageId
openTrade conn channel tradeData = do
    embed        <- makeOfferEmbed conn tradeData
    offerMessage <- messageId
        <$> restCall' (CreateMessageEmbed channel "" embed)
    setTrade conn offerMessage tradeData
    return offerMessage

-- | Open a random (useful!) trade.
randomTrade :: DB.Connection -> UserId -> DictM TradeData
randomTrade conn user = do
    demands    <- fromCredits <$> randomRIO (2, 10)
    n :: Float <- randomRIO (0.0, 1.0)
    offers     <- if
        | 0.0 <= n && n < 0.4 -> fromTrinket . fst <$> randomTrinket conn
        | 0.4 <= n && n < 1.1 -> fromCredits <$> randomRIO (3, 12)
        | otherwise           -> throwError $ Fuckup "unreachable"
    return $ TradeData OpenTrade offers demands user

-- embed       <-
--             maybe (throwError GTFO) return
--             . listToMaybe
--             . messageEmbeds
--             $ messageData
--         let isHandshake = (emojiName . reactionEmoji) react == "🤝"
--             isOpenOffer = embedTitle embed == Just openOfferDesc

--         when (isHandshake && isOpenOffer) $ do
--             let personReacting = reactionUserId react
--             personOffering <-
--                 getParsed
--                 . parseAuthor
--                 . fromMaybe ""
--                 . embedDescription
--                 $ embed
--             demandedItems <- getValue "Demands" embed
--             offeredItems  <- getValue "Offers" embed


--             if personOffering == personReacting
--                 then sendMessage
--                     channel
--                     "Do you believe I can't tell humans apart? You can't accept your own offer. It has been cancelled instead."
--                 else do
--                     ownsOrComplain conn personReacting demandedItems
--                     -- Manual error handling and ownership checks because trades are delayed.
--                     offersOwned <-
--                         flip userOwns offeredItems
--                             <$> getUserOr Fuckup conn personOffering
--                     if offersOwned
--                         then do
--                             let mention = "<@" <> show personOffering <> ">"
--                             sendMessage channel
--                                 $ "You don't have the goods you've put up for offer, "
--                                 <> mention
--                                 <> ". Your trade has been cancelled and your credits have been decremented."
--                             punishWallet conn personOffering
--                         else do
--                             takeItems conn personOffering offeredItems
--                             giveItems conn personReacting offeredItems
--                             takeItems conn personReacting demandedItems
--                             giveItems conn personOffering demandedItems
--                     return ()
--             let
--                 newEmbed = makeOfferEmbed False
--                                           personOffering
--                                           (offeredItems, demandedItems)
--             restCall'_ $ EditMessage (channel, message) "" (Just newEmbed)
--       where
--         message = reactionMessageId react
--         channel = reactionChannelId react

--         getValue value =
--             getParsed
--                 . parseItems
--                 . maybe "nothing" embedFieldValue
--                 . find ((== value) . embedFieldName)
--                 . embedFields

--         parseAuthor :: Text -> Either ParseError UserId
--         parseAuthor = parse parAuthor ""

--         parAuthor :: Parser UserId
--         parAuthor = do
--             void $ string "Offered by <@"
--             digits <- many1 digit
--             void $ string ">"
--             return . read $ digits
