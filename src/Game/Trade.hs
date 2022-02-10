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
import           Data.Default
import           Game                           ( decrementWallet
                                                , fromCredits
                                                , fromTrinket
                                                , fromUser
                                                , fromWord
                                                , giveItems
                                                , ownsOrComplain
                                                , takeItems
                                                , userOwns
                                                )
import           Game.Events                    ( randomTrinket )
import           Game.Items                     ( addItems )
import           System.Random
import           Utils                          ( oddsIO
                                                , randomWord
                                                )

tradeDesc :: TradeStatus -> Text
tradeDesc OpenTrade   = "Offer (OPEN: react with 🤝 to accept)"
-- tradeDesc PendingTrade = "Offer (PENDING)"
tradeDesc ClosedTrade = "Offer (CLOSED)"

tradeColour :: TradeStatus -> ColorInteger
tradeColour OpenTrade = 0x2ecc71
tradeColour _         = 0x888888

makeOfferEmbed :: TradeData -> DictM CreateEmbed
makeOfferEmbed tradeData = do
    let TradeData status offers demands offerer = tradeData
    showOffers  <- displayItems offers
    showDemands <- displayItems demands
    let offersDesc  = ("Offers", showOffers)
        demandsDesc = ("Demands", showDemands)
        descDesc    = "Offered by <@" <> show offerer <> ">"
        titleDesc   = tradeDesc status
        colour      = tradeColour status
    return $ mkEmbed titleDesc descDesc [offersDesc, demandsDesc] (Just colour)

handleTrade :: ChannelId -> MessageId -> TradeData -> UserId -> DictM ()
handleTrade channel message tradeData buyer = do
    let TradeData status offers demands seller = tradeData
    when (status == OpenTrade) $ if buyer == seller
        then do
            sendMessage
                channel
                "Do you believe I can't tell humans apart? You can't accept your own offer. It has been cancelled instead."
            cancelTrade channel message tradeData
        else do
            ownsOrComplain buyer demands
            -- Manual error handling and ownership checks because trades are delayed.
            offersOwned <-
                (|| seller == dictId)
                .   flip userOwns offers
                .   view userItems
                <$> getUser seller
            if not offersOwned
                then do
                    let mention = "<@" <> show seller <> ">"
                    sendMessage channel
                        $ "You don't have the goods you've put up for offer, "
                        <> mention
                        <> ". Your trade has been cancelled and your credits have been decremented."
                    decrementWallet seller
                else do
                    -- Catch and rethrow to avoid taking items. This is a bad way to do this but nobody will change it.
                    takeItems seller offers
                    runExceptT (lift $ giveItems buyer offers) >>= \case
                        Left  err -> giveItems seller offers >> throwError err
                        Right _   -> pure ()
                    takeItems buyer demands
                    runExceptT (lift $ giveItems seller demands) >>= \case
                        Left  err -> giveItems buyer demands >> throwError err
                        Right _   -> pure ()
                    sendMessage channel
                        $  "Transaction successful. Congratulations, <@"
                        <> show buyer
                        <> ">."
            cancelTrade channel message tradeData

cancelTrade :: ChannelId -> MessageId -> TradeData -> DictM ()
cancelTrade channel message trade = do
    let closedTrade = trade & tradeStatus .~ ClosedTrade
    embed <- makeOfferEmbed closedTrade
    setTrade message closedTrade
    restCall'_ $ EditMessage (channel, message) "" (Just embed)

openTrade :: ChannelId -> TradeData -> DictM MessageId
openTrade channel tradeData = do
    embed        <- makeOfferEmbed tradeData
    offerMessage <- messageId
        <$> restCall' (CreateMessageEmbed channel "" embed)
    setTrade offerMessage tradeData
    return offerMessage

-- | Open a random (useful!) trade.
randomTrade :: UserId -> DictM TradeData
randomTrade user = do
    demands <- oddsIO 0.8 >>= \b ->
        if b then fromCredits . round' <$> randomRIO (2, 10) else pure def
    offers <-
        randomRIO (1, 2) >>= flip replicateM randomOffer <&> foldr addItems def
    return $ TradeData OpenTrade offers demands user
  where
    randomOffer = do
        n :: Double <- randomIO
        if
            | n <= 0.20 -> fromTrinket . fst <$> randomTrinket
            | n <= 0.55 -> fromWord <$> liftIO randomWord
            | n <= 0.70 -> fromUser . userId . memberUser <$> randomMember
            | n <= 1.00 -> fromCredits . round' <$> randomRIO (3, 12)
            | otherwise -> throwError $ Fuckup "unreachable"
    round' =
        (/ 10) . (toEnum :: Int -> Double) . (round :: Double -> Int) . (* 10)
