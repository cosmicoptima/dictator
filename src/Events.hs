-- | ??? this file's purpose is unclear
module Events where

import           Relude                  hiding ( First )

-- local modules
----------------
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

--------
-- discord
----------
import           Discord
import           Discord.Types

-- color

-- all else
-----------
import           Control.Monad                  ( liftM2 )
import qualified Database.Redis                as DB
import           Discord.Internal.Rest.Channel  ( ChannelRequest
                                                    ( CreateMessageUploadFile
                                                    )
                                                )
import           System.Random


-- GPT
------

randomAdjective :: DictM Text
randomAdjective = liftIO $ liftM2 randomChoice getAdjList getStdGen

pontificate :: ChannelId -> Text -> DictM ()
pontificate channel what = do
    adj      <- randomAdjective
    response <-
        getJ1 32 $ "Dictator's " <> adj <> " thoughts on " <> what <> ":\n"
    sendMessage channel $ case lines response of
        (_ : line : _) -> line
        (line     : _) -> line
        _              -> response

dictate :: DictM ()
dictate = do
    adj    <- randomAdjective
    output <- getJ1FromContext
        32
        ("A " <> adj <> " forum dictator decrees the following")
        decrees
    case lines output of
        (message : _)
            | voiceFilter message `notElem` fmap voiceFilter decrees -> do
                sendMessageToGeneral message
        _ -> dictate
  where
    decrees =
        [ "i hereby decree that all members are forbidden from using the message board"
        , "i hereby declare my superiority over other posters"
        , "i hereby declare war upon the so-called \"elite\""
        , "i hereby decree my death"
        , "i hereby decree that credits shall be reinstated"
        , "i hereby decree that no members may use lowercase in their postings"
        , "i hereby declare ignorantism the official ideology"
        , "i hereby ban the user gotham"
        , "i hereby declare myself better than you"
        , "i hereby decree that no good deed shall remain unpunished"
        , "i hereby decree that the user gotham is a cute kitty"
        , "i hereby decree that gotham is owned"
        , "i hereby decree that pydict wasn't that bad, not really, i mean it kinda was, but come on"
        , "i hereby ban all mention, thought or knowledge of the mess i made this morning"
        , "i hereby command all of my subjects to earnestly praise me right now and whenever i'm feeling down in the future"
        , "i hereby declare gotham my heir, conditional on the permanence of his boyish charm"
        ]

postImage :: DictM ()
postImage = do
    image   <- randomImage
    word    <- liftIO randomWord
    general <- channelId <$> getGeneralChannel
    restCall'_ $ CreateMessageUploadFile general (word <> ".png") image

-- other
--------

stopDict :: DictM ()
stopDict = do
    conn <- asks envDb
    sendMessageToGeneral "I'm so tired..."
    liftIO $ DB.disconnect conn
    lift . lift $ stopDiscord
