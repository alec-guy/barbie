{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MinecraftTypes where


import Data.Vector as V
import Network.HTTP.Simple
import Data.Aeson
import Data.Text as T
import Data.Aeson.Types
import GHC.Generics
import Data.Scientific


{- To Prevent abusing the API, we must only
   get a minecraft box, which is huge, one time.
   So we call it here instead of the event handler.

-}

minecraftReq :: IO Network.HTTP.Simple.Request
minecraftReq = parseRequest $ T.unpack "https://minecraft-api.vercel.app/api/items"

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

instance FromJSON Minecraft
---------------------------------------------------
