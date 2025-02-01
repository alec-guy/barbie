{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands


help :: CreateApplicationCommand
help = CreateApplicationCommandChatInput
     {createName = "help"
     , createDescription = "display help information"
     , createLocalizedName = Nothing
     , createLocalizedDescription = Nothing
     , createOptions = Nothing
     , createDefaultMemberPermissions = Nothing
     , createDMPermission = Just False
     }

catimage :: CreateApplicationCommand
catimage = CreateApplicationCommandChatInput
         { createName = "cat"
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
      , createDescription = "a random heart emoji"
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
         { createName = "dog"
         , createDescription = "random dog image"
         , createLocalizedName = Nothing
         , createLocalizedDescription = Nothing
         , createOptions = Nothing
         , createDefaultMemberPermissions = Nothing
         , createDMPermission = Just False
         }
foximage :: CreateApplicationCommand
foximage = CreateApplicationCommandChatInput
         { createName = "fox"
         , createDescription = "random fox image"
         , createLocalizedName = Nothing
         , createLocalizedDescription = Nothing
         , createOptions = Nothing
         , createDefaultMemberPermissions = Nothing
         , createDMPermission = Just False
         }
minecraftR :: CreateApplicationCommand
minecraftR = CreateApplicationCommandChatInput
          { createName = "craft"
          , createDescription = "random minecraft item"
          , createLocalizedName = Nothing
          , createLocalizedDescription = Nothing
          , createOptions = Nothing
          , createDefaultMemberPermissions = Nothing
          , createDMPermission = Just False
          }

nasaYear :: OptionValue
nasaYear = OptionValueString
         {optionValueName = "year"
         ,optionValueLocalizedName = Nothing
         ,optionValueDescription = "year as number"
         ,optionValueLocalizedDescription = Nothing
         ,optionValueRequired = True
         , optionValueStringChoices = Left False
         , optionValueStringMinLen = Nothing
         , optionValueStringMaxLen = Just 10
         }
---------------------------------------------
nasaMonth :: OptionValue
nasaMonth = OptionValueString
         {optionValueName = "month"
         ,optionValueLocalizedName = Nothing
         ,optionValueDescription = "month as number"
         ,optionValueLocalizedDescription = Nothing
         ,optionValueRequired = True
         , optionValueStringChoices = Left False
         , optionValueStringMinLen = Nothing
         , optionValueStringMaxLen = Just 10
         }
nasaDay :: OptionValue
nasaDay = OptionValueString
         {optionValueName = "day"
         ,optionValueLocalizedName = Nothing
         ,optionValueDescription = "day as number"
         ,optionValueLocalizedDescription = Nothing
         ,optionValueRequired = True
         , optionValueStringChoices = Left False
         , optionValueStringMinLen = Nothing
         , optionValueStringMaxLen = Just 10
         }


nasa :: CreateApplicationCommand
nasa = CreateApplicationCommandChatInput
          { createName = "nasa"
          , createDescription = "nasa picture of the day"
          , createLocalizedName = Nothing
          , createLocalizedDescription = Nothing
          , createOptions = Just $ OptionsValues [nasaYear, nasaMonth, nasaDay]
          , createDefaultMemberPermissions = Nothing
          , createDMPermission = Just False
          }

------------------------------------------------
item :: OptionValue
item = OptionValueString
     {optionValueName = "name"
     , optionValueLocalizedName = Nothing
     , optionValueDescription = "item to search"
     , optionValueLocalizedDescription = Nothing
     , optionValueRequired = True
     , optionValueStringChoices = Left False
     , optionValueStringMinLen = Nothing
     , optionValueStringMaxLen = Just 25
     }

craftInfo :: CreateApplicationCommand
craftInfo = CreateApplicationCommandChatInput
          { createName = "craftinfo"
          , createDescription = "find info of minecraft item"
          , createLocalizedName = Nothing
          , createLocalizedDescription = Nothing
          , createOptions = Just $ OptionsValues [item]
          , createDefaultMemberPermissions = Nothing
          , createDMPermission = Just False
          }


pokemonName :: OptionValue
pokemonName = OptionValueString
            {optionValueName = "name"
            , optionValueLocalizedName = Nothing
            , optionValueDescription = "pokemon to search for"
            , optionValueLocalizedDescription = Nothing
            , optionValueRequired = True
            , optionValueStringChoices = Left False
            , optionValueStringMinLen = Nothing
            , optionValueStringMaxLen = Just 25
            }
pokemon :: CreateApplicationCommand
pokemon = CreateApplicationCommandChatInput
        { createName = "pokedex"
        , createDescription = "find info of pokemon"
        , createLocalizedName = Nothing
        , createLocalizedDescription = Nothing
        , createOptions = Just $ OptionsValues [pokemonName]
        , createDefaultMemberPermissions = Nothing
        , createDMPermission = Just False
        }
