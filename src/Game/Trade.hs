{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE QuasiQuotes            #-}

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
import qualified Data.MultiSet                 as MS
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Game                           ( decrementWallet
                                                , fromCredits
                                                , fromRole
                                                , fromTrinket
                                                , fromUser
                                                , fromWord
                                                , giveItems
                                                , ownsOrComplain
                                                , takeItems
                                                , userOwns
                                                )
import           Game.Events                    ( randomTrinket )
import           Game.Items                     ( Items
                                                , addItems
                                                , itemCredits
                                                , itemRoles
                                                , itemTrinkets
                                                , itemUsers
                                                , itemWords
                                                )
import           Game.Roles                     ( lookupRole
                                                , randomColoredRole
                                                )
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
    demands <- oddsIO 0.8 >>= \b -> if b
        then fromCredits . round' <$> randomRIO (2, 10)
        else fromRole <$> randColor
    offers <-
        randomRIO (1, 2) >>= flip replicateM randomOffer <&> foldr addItems def
    return $ TradeData OpenTrade offers demands user
  where
    randomOffer = do
        n :: Double <- randomIO
        if
            | n <= 0.15 -> fromTrinket . fst <$> randomTrinket
            | n <= 0.55 -> fromWord <$> liftIO randomWord
            | n <= 0.65 -> fromUser . userId . memberUser <$> randomMember
            | n <= 0.95 -> fromRole <$> randColor
            | n <= 1.00 -> fromCredits . round' <$> randomRIO (12, 50)
            | otherwise -> throwError $ Fuckup "unreachable"
    round' =
        (/ 10) . (toEnum :: Int -> Double) . (round :: Double -> Int) . (* 10)
    randColor = roleColor <$> randomColoredRole

displayItems :: Items -> DictM Text
displayItems it = do
    trinketsDisplay <- showTrinkets (it ^. itemTrinkets . to MS.elems)
    rolesDisplay    <- showRoles (it ^. itemRoles . to MS.elems)
    let wordsDisplay = fmap show (it ^. itemWords . to MS.elems)
        usersDisplay = fmap showUser (it ^. itemUsers . to MS.elems)
        display =
            T.intercalate ", "
                .  filter (not . T.null)
                $  showCredits (it ^. itemCredits)
                :  wordsDisplay
                ++ rolesDisplay
                ++ trinketsDisplay
                ++ usersDisplay
    return $ if display == "" then "nothing" else display
  where
    showCredits 0 = ""
    showCredits n = show n <> "c"

    showTrinkets = mapM $ \trinketId -> do
        trinketData <- getTrinketOr Complaint trinketId
        displayTrinket trinketId trinketData
    showRoles = mapM $ \roleCol -> do
        role <-
            lookupRole roleCol
                >>= maybe
                        (throwError $ Fuckup
                            [i|Role with col #{roleCol} no longer exists!|]
                        )
                        return
        return [i|<@&#{roleId role}>|]
    showUser = ("<@!" <>) . (<> ">") . show
