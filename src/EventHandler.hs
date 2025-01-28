{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module EventHandler where

import Data.Text as T
import Data.Text.IO as TIO
import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Events
import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Embed
import Discord.Internal.Types.Color
import Control.Monad.Reader
import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Control.Exception
import Data.Vector as V
import Data.Aeson.KeyMap
import Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Scientific


data Joke = Joke Text Text Text Text deriving (Show)

instance FromJSON Joke where
    parseJSON (Object obj) = do
       let maptext = toMapText obj
           typ            = M.lookup "type" maptext
           setup          = M.lookup "setup" maptext
           punchline      = M.lookup "punchline" maptext
           identification = M.lookup "id" maptext
       case (typ,setup,punchline,identification) of
          (Just (String t), Just (String s), Just (String p), Just (Number i)) ->
                       return $ Joke t s p (T.pack $ show $ fromJust $ (toBoundedInteger i :: Maybe Int))
          _                                        -> return $ Joke "" "" "" ""


data Cat = Cat {
         breeds :: [Text],
         id     :: Text,
         url    :: Text,
         width  :: Int,
         height :: Int
        } deriving (Generic, Show)
emptyCat = Cat {breeds = [], id = "",url = "",width = 0,height = 0}

instance FromJSON Cat

newtype CatArray = CatArray [Cat] deriving (Show)

instance FromJSON CatArray where
  parseJSON (Array vector) = do
        let monadVector = do
                 obj <- vector
                 return $ ((parseJSON obj) :: Parser Cat)
        cats <- Prelude.sequence $ V.toList $ monadVector
        return $ CatArray cats



jokeRequest :: IO Network.HTTP.Simple.Request
jokeRequest = parseRequest $ T.unpack "https://official-joke-api.appspot.com/random_joke"

catRequest :: Text -> IO Network.HTTP.Simple.Request
catRequest ctok = parseRequest $ T.unpack ("https://api.thecatapi.com/v1/images/search?limit=1&api_key=" `T.append` ctok)

jokeMsg :: Text -> Text -> Text -> InteractionResponseMessage
jokeMsg typ setup punch =
    let joke = setup `T.append` "\n" `T.append` punch
    in  interactionResponseMessageBasic joke

heartMessage :: InteractionResponseMessage
heartMessage = interactionResponseMessageBasic "❤️"


catMsg :: Text ->  Int -> Int -> InteractionResponseMessage
catMsg urlCat width height = InteractionResponseMessage
              { interactionResponseMessageTTS = Nothing
              , interactionResponseMessageContent = Nothing
              , interactionResponseMessageEmbeds = Just [catEmbed urlCat width height]
              , interactionResponseMessageAllowedMentions = Nothing
              , interactionResponseMessageFlags = Nothing
              , interactionResponseMessageComponents = Nothing
              , interactionResponseMessageAttachments = Nothing
              }

catEmbed :: Text -> Int -> Int -> CreateEmbed
catEmbed caturl width height = CreateEmbed { createEmbedAuthorName = "Barbie Cat"
                              , createEmbedAuthorUrl = ""
                              , createEmbedAuthorIcon = Nothing
                              , createEmbedTitle = ""
                              , createEmbedUrl   =  caturl
                              , createEmbedThumbnail = Nothing
                              , createEmbedDescription= ""
                              , createEmbedFields = []
                              , createEmbedImage = Just $ CreateEmbedImageUrl caturl
                              , createEmbedFooterText = let ta = T.append
                                                            tw = T.pack $ show width
                                                            th = T.pack $ show height
                                                        in tw `ta` " x " `ta` th
                              , createEmbedFooterIcon = Nothing
                              , createEmbedColor = Just $ DiscordColorRed
                              , createEmbedTimestamp = Nothing
                              }
eventHandler :: Text -> Event -> DiscordHandler ()
eventHandler ctok (InteractionCreate interaction) =
  case interaction of
    InteractionApplicationCommand {interactionId = iId, interactionUser = u, interactionToken = tokId, applicationCommandData = appCommData} ->
      case appCommData of
        ApplicationCommandDataChatInput {applicationCommandDataName = commandname, ..} ->
          case commandname of
            "catimage" -> do
                           let catresponse = liftIO $ do
                                                       req  <- catRequest ctok
                                                       resp <- catch ((Right <$> httpJSON req) :: IO (Either () (Response CatArray))) (\e -> do
                                                                                              Prelude.putStrLn $ (show (e :: SomeException))
                                                                                              return $ Left ())
                                                       return resp
                           maybeR <- catresponse
                           case maybeR of
                            Left () -> return ()
                            Right ca ->  do
                                          let catarray = getResponseBody ca
                                          case catarray of
                                           CatArray cats -> do
                                             let cat= Prelude.head cats
                                             rc <- let ctmsg = catMsg (url cat) (width cat) (height cat)
                                                   in restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage ctmsg ))
                                             case rc of
                                              Left code -> lift $ Prelude.putStrLn (show code)

                                              Right _   -> return ()
            "heart"   -> do
                          rc <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage heartMessage))
                          case rc of
                           Left code' -> lift $ Prelude.putStrLn (show code')
                           Right _    -> return ()
            "joke"    -> do
                          let jokeresponse = liftIO $ do
                                                       req <- jokeRequest
                                                       resp <- catch ((Right <$> httpJSON req) :: IO (Either () (Response Joke))) (\e -> do
                                                                                              Prelude.putStrLn $ (show (e :: SomeException))
                                                                                              return $ Left ())
                                                       return resp
                          maybeR <- jokeresponse
                          case maybeR of
                           Left () -> return ()
                           Right jr -> do
                                        let jr' = getResponseBody jr
                                        case jr' of
                                         (Joke typ setup punch id') -> do
                                                 r <-restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage (jokeMsg typ setup punch)))
                                                 case r of
                                                  Left e -> lift $ Prelude.putStrLn (show e)
                                                  Right _ -> return ()
            t         -> lift $ TIO.putStrLn $ "Command user entered = " `T.append` t
        _ -> lift $ TIO.putStrLn "Different type of app comm data"
    _ -> lift $ TIO.putStrLn "Different type of interaction"
