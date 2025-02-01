module Gateway where

import Discord
import Discord.Internal.Types.Gateway

gateway =
    GatewayIntent
    { gatewayIntentGuilds = True
    , gatewayIntentMembers = True
    , gatewayIntentBans    = False
    , gatewayIntentEmojis = False
    , gatewayIntentIntegrations = False
    , gatewayIntentWebhooks = False
    , gatewayIntentInvites = False
    , gatewayIntentVoiceStates = False
    , gatewayIntentPresences = True
    , gatewayIntentMessageChanges = False
    , gatewayIntentMessageReactions = False
    , gatewayIntentMessageTyping    = False
    , gatewayIntentDirectMessageChanges =  False
    , gatewayIntentDirectMessageReactions = False
    , gatewayIntentDirectMessageTyping = False
    , gatewayIntentMessageContent = True
    --, gatewayIntentAutoModerationConfiguration = False
    --, gatewayIntentAutoModerationExecution = False
    }
