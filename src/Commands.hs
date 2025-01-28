{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands

catimage :: CreateApplicationCommand
catimage = CreateApplicationCommandChatInput
         { createName = "catimage"
         , createDescription = "random cat image"
         , createLocalizedName = Nothing
         , createLocalizedDescription = Nothing
         , createOptions = Nothing
         , createDefaultMemberPermissions = Nothing
         , createDMPermission = Just False
         }

heart :: CreateApplicationCommand
heart = CreateApplicationCommandChatInput
      {createName = "heart"
      , createDescription = "an emoji"
      , createLocalizedName = Nothing
      , createLocalizedDescription = Nothing
      , createOptions = Nothing
      , createDefaultMemberPermissions = Nothing
      , createDMPermission = Just False
      }
joke :: CreateApplicationCommand
joke = CreateApplicationCommandChatInput
      { createName = "joke"
      , createDescription = "a random joke"
      , createLocalizedName = Nothing
      , createLocalizedDescription = Nothing
      , createOptions = Nothing
      , createDefaultMemberPermissions = Nothing
      , createDMPermission = Just False
      }
dogimage :: CreateApplicationCommand
dogimage = CreateApplicationCommandChatInput
         { createName = "dogimage"
         , createDescription = "random dog image"
         , createLocalizedName = Nothing
         , createLocalizedDescription = Nothing
         , createOptions = Nothing
         , createDefaultMemberPermissions = Nothing
         , createDMPermission = Just False
         }
foximage :: CreateApplicationCommand
foximage = CreateApplicationCommandChatInput
         { createName = "foximage"
         , createDescription = "random fox image"
         , createLocalizedName = Nothing
         , createLocalizedDescription = Nothing
         , createOptions = Nothing
         , createDefaultMemberPermissions = Nothing
         , createDMPermission = Just False
         }
