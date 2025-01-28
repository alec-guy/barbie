module DiscordHandler where

import Control.Monad (sequence)
import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.ApplicationInfo
import Control.Monad.Reader
import Data.Map as M

discordHandler :: [CreateApplicationCommand] -> DiscordHandler ()
discordHandler commands =  do
    lift $ putStrLn "Barbie Bot version 1.0.0"
    cache <- readCache
    let fullapp   = cacheApplication cache
        appid     = fullApplicationID fullapp
        guildIds  = keys $ cacheGuilds cache

    sequence_ ((\command -> restCall $ CreateGlobalApplicationCommand appid command) <$> commands)
