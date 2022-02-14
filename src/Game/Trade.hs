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
import           Game.Roles                     ( randomColoredRole
                                                , showRole
                                                , updateUserRoles
                                                )
import           System.Random
import           Utils                          ( randomWord
                                                , showMultiSet
                                                )

tradeDesc :: TradeStatus -> Text
tradeDesc OpenTrade    = "Offer (OPEN: react with 🤝 to accept)"
tradeDesc PendingTrade = "Offer (PENDING...)"
tradeDesc ClosedTrade  = "Offer (CLOSED)"

tradeColour :: TradeStatus -> ColorInteger
tradeColour OpenTrade    = 0x2ecc71
tradeColour PendingTrade = 0x3498db
tradeColour _            = 0x888888

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
      updateTradeStatus ClosedTrade channel message tradeData
    else do
      runExceptT
          (lift $ do
            updateTradeStatus PendingTrade channel message tradeData
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
                -- Since a trade happened, we should update both user's roles.
                updateUserRoles seller
                updateUserRoles buyer
          )
        >>= \case
              Left err -> do
                updateTradeStatus OpenTrade channel message tradeData
                throwError err
              Right _ ->
                updateTradeStatus ClosedTrade channel message tradeData

updateTradeStatus
  :: TradeStatus -> ChannelId -> MessageId -> TradeData -> DictM ()
updateTradeStatus status channel message trade = do
  let changedTrade = trade & tradeStatus .~ status
  embed <- makeOfferEmbed changedTrade
  setTrade message changedTrade
  restCall'_ $ EditMessage (channel, message) "" (Just embed)

createTrade :: ChannelId -> TradeData -> DictM MessageId
createTrade channel tradeData = do
  embed        <- makeOfferEmbed tradeData
  offerMessage <- messageId <$> restCall' (CreateMessageEmbed channel "" embed)
  setTrade offerMessage tradeData
  return offerMessage

-- | Open a random (useful!) trade.
randomTrade :: UserId -> DictM TradeData
randomTrade user = do
  demands <-
    randomRIO (1, 2) >>= flip replicateM randomDemand <&> foldr addItems def
  offers <-
    randomRIO (1, 3) >>= flip replicateM randomOffer <&> foldr addItems def
  return $ TradeData OpenTrade offers demands user
 where
  randomOffer = do
    n :: Double <- randomIO
    if
      | n <= 0.10 -> fromTrinket . fst <$> randomTrinket
      | n <= 0.45 -> fromWord <$> liftIO randomWord
      | n <= 0.55 -> fromUser . userId . memberUser <$> randomMember
      | n <= 0.95 -> fromRole <$> randColor
      | n <= 1.00 -> fromCredits . round' <$> randomRIO (20, 50)
      | otherwise -> throwError $ Fuckup "unreachable"
  randomDemand = do
    n :: Double <- randomIO
    if
      | n <= 0.65 -> fromCredits . round' <$> randomRIO (2, 10)
      | n <= 1.00 -> fromRole <$> randColor
      | otherwise -> throwError $ Fuckup "unreachable"

  round' =
    (/ 10) . (toEnum :: Int -> Double) . (round :: Double -> Int) . (* 10)
  randColor = roleColor <$> randomColoredRole

displayItems :: Items -> DictM Text
displayItems it = do
  trinketsDisplay <- showTrinkets (it ^. itemTrinkets . to MS.elems)
  rolesDisplay    <- showMultiSet <$> multiMapM showRole (it ^. itemRoles)
  let wordsDisplay = it ^. itemWords . to (showMultiSet . MS.map show)
      usersDisplay = it ^. itemUsers . to (showMultiSet . MS.map showUser)
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
  showUser = ("<@!" <>) . (<> ">") . show

  multiMapM f l = MS.fromList <$> mapM f (MS.elems l)
