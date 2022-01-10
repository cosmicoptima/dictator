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
import qualified Database.Redis                as DB
import           Game                           ( decrementWallet
                                                , giveItems
                                                , ownsOrComplain
                                                , takeItems
                                                , userOwns
                                                )
import           Game.Items

tradeDesc :: TradeStatus -> Text
tradeDesc OpenTrade   = "Offer (OPEN: react with 🤝 to accept)"
-- tradeDesc PendingTrade = "Offer (PENDING)"
tradeDesc ClosedTrade = "Offer (CLOSED)"

tradeColour :: TradeStatus -> ColorInteger
tradeColour OpenTrade = 0x2ecc71
tradeColour _         = 0x888888

makeOfferEmbed :: TradeData -> CreateEmbed
makeOfferEmbed tradeData =
    let TradeData status offers demands offerer = tradeData
        offersDesc  = ("Offers", fromString $ showItems offers)
        demandsDesc = ("Demands", fromString $ showItems demands)
        descDesc    = "Offered by <@" <> show offerer <> ">"
        titleDesc   = tradeDesc status
        colour      = tradeColour status
    in  mkEmbed titleDesc descDesc [demandsDesc, offersDesc] $ Just colour

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
    setTrade conn message closedTrade
    restCall'_ $ EditMessage (channel, message)
                             ""
                             (Just $ makeOfferEmbed closedTrade)



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
