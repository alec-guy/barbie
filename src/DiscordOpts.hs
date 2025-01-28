module DiscordOpts where

import Discord

import DiscordHandler
import Gateway
import EventHandler
import Data.Text
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands


myDiscordOpts :: Text -> Text -> [CreateApplicationCommand] -> RunDiscordOpts
myDiscordOpts token cattoken globalCommands = RunDiscordOpts
                    { discordToken = token
                    , discordOnStart = discordHandler globalCommands
                    , discordOnEnd = return ()
                    , discordOnEvent = eventHandler cattoken
                    , discordOnLog = \_ -> return ()
                    , discordForkThreadForEvents  = False
                    , discordGatewayIntent = gateway
                    , discordEnableCache = False
                    }
