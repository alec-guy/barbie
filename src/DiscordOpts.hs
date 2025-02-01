module DiscordOpts where

import Discord

import DiscordHandler
import Gateway
import EventHandler
import Data.Text
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands
import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Types
import Data.Vector as V
import MinecraftTypes
import Control.Exception
import Control.Monad.Reader

myDiscordOpts :: Text -> Text -> Text -> [CreateApplicationCommand] -> RunDiscordOpts
myDiscordOpts token cattoken nasa globalCommands = RunDiscordOpts
                    { discordToken = token
                    , discordOnStart = discordHandler globalCommands
                    , discordOnEnd = return ()
                    , discordOnEvent = \e -> do
                                    let mcR = liftIO  $ do
                                                         req <- minecraftReq
                                                         resp <- catch ((Right <$> httpJSON req) :: IO (Either () (Response MinecraftBox))) (\e -> do
                                                                                                  Prelude.putStrLn $ (show (e ::  SomeException))
                                                                                                  return $ Left ())
                                                         return resp
                                    maybeR <- mcR
                                    case maybeR of
                                     Left () -> eventHandler cattoken nasa (MinecraftBox V.empty) e
                                     Right mr ->
                                           do
                                            let box   = getResponseBody mr
                                            eventHandler cattoken nasa box e

                    , discordOnLog = \_ -> return ()
                    , discordForkThreadForEvents  = False
                    , discordGatewayIntent = gateway
                    , discordEnableCache = False
                    }
