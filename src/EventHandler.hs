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
import System.Random
import Data.Either (fromRight)

fromDigit :: Text -> Text
fromDigit "1" = "01"
fromDigit "2" = "02"
fromDigit "3" = "03"
fromDigit "4" = "04"
fromDigit "5" = "05"
fromDigit "6" = "06"
fromDigit "7" = "07"
fromDigit "8" = "08"
fromDigit "9" = "09"
fromDigit s   = s

data Nasa = Nasa
          { concepts :: Text
          , date :: Text
          , explanation :: Text
          , hdurl :: Text
          , mediaType :: Text
          , serviceVersion :: Text
          , title :: Text
          , urll :: Text
          } deriving (Show)

instance FromJSON Nasa where
    parseJSON (Object obj) = do
      let maptext = toMapText obj
          conc    =  case (fromJust $ M.lookup "concepts" maptext) of
                      (String x) -> x
                      _          -> error "concepts is not a string"

          date'    =  case (fromJust $ M.lookup "date" maptext) of
                       (String x) -> x
                       _          -> error "date is not a string"
          explanation' = case fromJust $ M.lookup "explanation" maptext of
                          (String x) -> x
                          _          -> error "explanation is not a string"
          hdurl'   =  case fromJust $ M.lookup "hdurl" maptext of
                       (String x) -> x
                       _          -> error "hdurl is not a string"

          mediatype = case fromJust $ M.lookup "media_type" maptext of
                       (String x) -> x
                       _          -> error "media_type is not a string"

          serviceversion = case fromJust $ M.lookup "service_version" maptext of
                            (String x) -> x
                            _          -> error "service_version is not a string"
          title'    = case fromJust $ M.lookup "title" maptext of
                       (String x) -> x
                       _          -> error "title is not a string"
          url'     = case fromJust $ M.lookup "url" maptext of
                      (String x) -> x
                      _          -> error "url is not a string"

      return $ Nasa {concepts = conc
                    ,date = date'
                    ,explanation = explanation'
                    ,hdurl       = hdurl'
                    , mediaType = mediatype
                    , serviceVersion = serviceversion
                    , title = title'
                    , urll = url'
                    }



data Dog = Dog {message :: Text, status :: Text} deriving (Show, Generic)

instance FromJSON Dog

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

data Fox = Fox Text deriving (Show, Generic)

instance FromJSON Fox where
   parseJSON (Object obj) = do
    let maptext = toMapText obj
        image'  = M.lookup "image" maptext
    case image' of
     (Just (String i)) ->
        return $ Fox i


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

newtype MinecraftBox = MinecraftBox (Vector Minecraft) deriving (Show)

instance FromJSON MinecraftBox where
  parseJSON (Array vector) = do
      let monadVector = do
           obj <- vector
           return $ ((parseJSON obj) :: Parser Minecraft)
      minecrafts <- V.sequence monadVector
      return $ MinecraftBox minecrafts

data Minecraft = Minecraft
               { name :: Text
               , namespacedId :: Text
               , description :: Text
               , image :: Text
               , stackSize :: Int
               , renewable :: Bool
               } deriving (Show, Generic)

hasMinecraftName :: Text -> Minecraft -> Bool
hasMinecraftName n mc = (name mc) == n

instance FromJSON Minecraft

minecraftMessageContent :: Minecraft -> Text
minecraftMessageContent mc =
       let ta = T.append
       in "Name: " `ta` (name mc) `ta` "\nDescription: " `ta` (description mc) `ta` "\nStack size: " `ta` (T.pack $ show $ stackSize mc)
nasaMessageContent :: Nasa -> Text
nasaMessageContent nasa =
        let ta = T.append
        in  "Date: " `ta` (date nasa) `ta` "\nExplanation: " `ta` (explanation nasa)


nasaReq :: Text -> Text -> IO Network.HTTP.Simple.Request
nasaReq key date =
  parseRequest $ T.unpack  $ "https://api.nasa.gov/planetary/apod?api_key=" `T.append` key `T.append` "&date=" `T.append` date `T.append` "&concept_tags=True"

nasaMsg :: Nasa -> InteractionResponseMessage
nasaMsg nasa = InteractionResponseMessage
             { interactionResponseMessageTTS = Nothing
             , interactionResponseMessageContent = Just $ nasaMessageContent nasa
             , interactionResponseMessageEmbeds = Just [nasaEmbed (urll nasa)]
             , interactionResponseMessageAllowedMentions = Nothing
             , interactionResponseMessageFlags           = Nothing
             , interactionResponseMessageComponents      = Nothing
             , interactionResponseMessageAttachments     = Nothing
             }
nasaEmbed :: Text -> CreateEmbed
nasaEmbed imageurl = CreateEmbed
                { createEmbedAuthorName = "Barbie NASA"
                , createEmbedAuthorUrl  = ""
                , createEmbedAuthorIcon = Nothing
                , createEmbedUrl        = imageurl
                , createEmbedTitle      = ""
                , createEmbedThumbnail  = Nothing
                , createEmbedFields     = []
                , createEmbedImage = Just $ CreateEmbedImageUrl imageurl
                , createEmbedFooterText  = ""
                , createEmbedFooterIcon = Nothing
                , createEmbedColor = Just $ (DiscordColorRGB 224 33 138)
                , createEmbedTimestamp = Nothing
                , createEmbedDescription = ""
                }



minecraftReq :: IO Network.HTTP.Simple.Request
minecraftReq = parseRequest $ T.unpack "https://minecraft-api.vercel.app/api/items"

minecraftMsg :: Minecraft -> InteractionResponseMessage
minecraftMsg minecraft = InteractionResponseMessage
                       { interactionResponseMessageTTS = Nothing
                       , interactionResponseMessageContent = Just $ minecraftMessageContent minecraft
                       , interactionResponseMessageEmbeds = Just [minecraftEmbed (image minecraft)]
                       , interactionResponseMessageAllowedMentions = Nothing
                       , interactionResponseMessageFlags           = Nothing
                       , interactionResponseMessageComponents      = Nothing
                       , interactionResponseMessageAttachments     = Nothing
                       }

minecraftEmbed :: Text -> CreateEmbed
minecraftEmbed imageurl = CreateEmbed
                { createEmbedAuthorName = "Barbie Minecraft"
                , createEmbedAuthorUrl  = ""
                , createEmbedAuthorIcon = Nothing
                , createEmbedUrl        = imageurl
                , createEmbedTitle      = ""
                , createEmbedThumbnail  = Nothing
                , createEmbedFields     = []
                , createEmbedImage = Just $ CreateEmbedImageUrl imageurl
                , createEmbedFooterText  = ""
                , createEmbedFooterIcon = Nothing
                , createEmbedColor = Just $ (DiscordColorRGB 224 33 138)
                , createEmbedTimestamp = Nothing
                , createEmbedDescription = ""
                }


foxRequest :: IO Network.HTTP.Simple.Request
foxRequest = parseRequest $ T.unpack "https://randomfox.ca/floof"

dogRequest :: IO Network.HTTP.Simple.Request
dogRequest = parseRequest $ T.unpack "https://dog.ceo/api/breeds/image/random"

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

foxEmbed :: Text -> CreateEmbed
foxEmbed foxurl = CreateEmbed
                { createEmbedAuthorName = "Barbie Fox"
                , createEmbedAuthorUrl  = ""
                , createEmbedAuthorIcon = Nothing
                , createEmbedUrl        = foxurl
                , createEmbedTitle      = ""
                , createEmbedThumbnail  = Nothing
                , createEmbedFields     = []
                , createEmbedImage = Just $ CreateEmbedImageUrl foxurl
                , createEmbedFooterText  = ""
                , createEmbedFooterIcon = Nothing
                , createEmbedColor = Just $ (DiscordColorRGB 224 33 138)
                , createEmbedTimestamp = Nothing
                , createEmbedDescription = ""
                }


foxMsg :: Text -> InteractionResponseMessage
foxMsg foxurl = InteractionResponseMessage
              { interactionResponseMessageTTS = Nothing
              , interactionResponseMessageContent = Nothing
              , interactionResponseMessageEmbeds = Just [foxEmbed foxurl]
              , interactionResponseMessageAllowedMentions = Nothing
              , interactionResponseMessageFlags           = Nothing
              , interactionResponseMessageComponents      = Nothing
              , interactionResponseMessageAttachments     = Nothing
              }
dogMsg :: Text -> InteractionResponseMessage
dogMsg dogurl = InteractionResponseMessage
              { interactionResponseMessageTTS = Nothing
              , interactionResponseMessageContent = Nothing
              , interactionResponseMessageEmbeds = Just [dogEmbed dogurl]
              , interactionResponseMessageAllowedMentions = Nothing
              , interactionResponseMessageFlags           = Nothing
              , interactionResponseMessageComponents      = Nothing
              , interactionResponseMessageAttachments     = Nothing
              }

dogEmbed :: Text -> CreateEmbed
dogEmbed dogurl = CreateEmbed
                { createEmbedAuthorName = "Barbie Dog"
                , createEmbedAuthorUrl  = ""
                , createEmbedAuthorIcon = Nothing
                , createEmbedUrl        = dogurl
                , createEmbedTitle      = ""
                , createEmbedThumbnail  = Nothing
                , createEmbedFields     = []
                , createEmbedImage = Just $ CreateEmbedImageUrl dogurl
                , createEmbedFooterText  = ""
                , createEmbedFooterIcon = Nothing
                , createEmbedColor = Just $ (DiscordColorRGB 224 33 138)
                , createEmbedTimestamp = Nothing
                , createEmbedDescription = ""
                }
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
                              , createEmbedColor = Just $ (DiscordColorRGB 224 33 138)
                              , createEmbedTimestamp = Nothing
                              }
eventHandler :: Text -> Text -> Event -> DiscordHandler ()
eventHandler ctok nasatok (InteractionCreate interaction) =
  case interaction of
    InteractionApplicationCommand {interactionId = iId, interactionUser = u, interactionToken = tokId, applicationCommandData = appCommData} ->
      case appCommData of
        ApplicationCommandDataChatInput {applicationCommandDataName = commandname,optionsData = opt, ..} ->
          case commandname of
            "nasa"  -> do
                        case opt of
                         Nothing -> return ()
                         (Just (OptionsDataValues l)) ->
                            case l of
                              ([OptionDataValueString{optionDataValueString = y, optionDataValueName = n1},OptionDataValueString{optionDataValueString = m,optionDataValueName = n2},OptionDataValueString{optionDataValueString= d,optionDataValueName = n3}]) ->
                                       do
                                        let nr = liftIO $ do
                                                           let year = fromRight "" y
                                                               month = fromDigit $ fromRight "" m
                                                               day   = fromDigit $ fromRight "" d
                                                           req  <- nasaReq nasatok ((year) `T.append` "-" `T.append` ((month)) `T.append` "-" `T.append` ((day)))
                                                           resp <-  catch ((Right <$> httpJSON req) :: IO (Either () (Response Nasa))) (\e -> do

                      Prelude.putStrLn $ (show (e ::  SomeException))

                      return $ Left ())

                                                           return resp
                                        maybeN <- nr
                                        case maybeN of
                                         Left () -> return ()
                                         Right n ->
                                               do
                                                let nasa = getResponseBody n
                                                nasaRC   <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage $nasaMsg nasa))
                                                return ()
                              t
      -> lift $ Prelude.putStrLn $ "I was given something else for nasa"  Prelude.++ (show t)

                         _                            -> return ()

            "craft" -> do
                                    let mcR = liftIO $ do
                                                        req <- minecraftReq
                                                        resp <- catch ((Right <$> httpJSON req) :: IO (Either () (Response MinecraftBox))) (\e -> do

                      Prelude.putStrLn $ (show (e ::  SomeException))

                      return $ Left ())
                                                        return resp
                                    maybeR <- mcR
                                    case maybeR of
                                     Left () -> return ()
                                     Right mr ->
                                           do
                                            let box   = getResponseBody mr
                                                ionum = lift $ do
                                                          case box of
                                                           (MinecraftBox box') -> do
                                                                  gen <- getStdGen
                                                                  let (num , _) = randomR (0, (V.length box') - 1) gen
                                                                  return num

                                                iomc  = do
                                                         case box of
                                                          (MinecraftBox box') -> do
                                                             num <- ionum
                                                             return (box' V.! num)
                                            mc <- iomc
                                            rc <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage $ minecraftMsg mc))
                                            case rc of
                                             Left code -> lift $ Prelude.putStrLn (show code)
                                             Right _   -> return ()
            "craftinfo" -> do
                              let mcR = liftIO $ do
                                                  req <- minecraftReq
                                                  resp <- catch ((Right <$> httpJSON req) :: IO (Either () (Response MinecraftBox))) (\e -> do

                      Prelude.putStrLn $ (show (e ::  SomeException))

                      return $ Left ())
                                                  return resp
                              maybeR <- mcR
                              case maybeR of
                               Left () -> return ()
                               Right mr -> do
                                    let box = case (getResponseBody mr) of
                                               (MinecraftBox box') -> box'
                                    case opt of
                                     Nothing -> return ()
                                     (Just (OptionsDataValues l)) ->
                                       case l of
                                        [OptionDataValueString{optionDataValueString = eitemName, optionDataValueName = n1}] -> do
                                          let  itemName = fromRight "" eitemName
                                               maybeItem = V.find (hasMinecraftName itemName) box
                                          case maybeItem of
                                           Nothing -> do
                                                       rc <- restCall(CreateInteractionResponse iId tokId (InteractionResponseChannelMessage $ interactionResponseMessageBasic "Sorry, I could not find that item :p"))
                                                       return ()
                                           (Just mine) -> do
                                                           rc <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage $ minecraftMsg mine))
                                                           return ()



            "dogimage" -> do
                           let dogresponse = liftIO $ do
                                                       req <- dogRequest
                                                       resp <-  catch ((Right <$> httpJSON req) :: IO (Either () (Response Dog))) (\e -> do

                  Prelude.putStrLn $ (show (e :: SomeException))

                  return $ Left ())
                                                       return resp
                           maybeR <- dogresponse
                           case maybeR of
                            Left () -> return ()
                            Right dr -> do
                                          let dog    = getResponseBody dr
                                              dogurl = message dog
                                              dogmsg = dogMsg dogurl
                                          rc <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage dogmsg))
                                          case rc of
                                           Left code -> lift $ Prelude.putStrLn (show code)
                                           Right _   -> return ()
            "foximage" ->  do
                            let foxresponse = liftIO $ do
                                                        req <- foxRequest
                                                        resp <-  catch ((Right <$> httpJSON req) :: IO (Either () (Response Fox))) (\e -> do

                  Prelude.putStrLn $ (show (e :: SomeException))

                  return $ Left ())
                                                        return resp
                            maybeR <- foxresponse
                            case maybeR of
                             Left () -> return ()
                             Right fr -> do
                                          let fox    = getResponseBody fr
                                              foxurl = case fox of
                                                        (Fox furl) -> furl
                                              foxmsg = foxMsg foxurl
                                          rc <- restCall (CreateInteractionResponse iId tokId (InteractionResponseChannelMessage foxmsg))
                                          case rc of
                                           Left code -> lift $ Prelude.putStrLn (show code)
                                           Right _   -> return ()



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
