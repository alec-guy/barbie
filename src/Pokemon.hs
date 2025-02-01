{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Pokemon where


import Data.Vector as V
import Data.Aeson
import Data.Text as T
import Data.Aeson.Types
import GHC.Generics
import Data.Map as M
import Data.Aeson.KeyMap
import Data.Scientific
import Data.Maybe (fromJust)
import Control.Monad as Monad
import Data.List as L


data Type    =  Type
             { typeName :: Text
             } deriving (Show, Eq)

showType :: Type -> Text
showType = typeName

showTypes :: [Type] -> Text
showTypes types =  L.foldr (<>) T.empty (L.intersperse "," (Prelude.map showType types))

instance FromJSON Type where
  parseJSON (Object km) = do
     let mapText = toMapText km
         typeN   = case M.lookup "type" mapText of
                    Nothing -> ""
                    (Just (Object km2)) -> do
                           let mapText2 = toMapText km2
                           case M.lookup "name" mapText2 of
                            Nothing -> ""
                            (Just (String n)) -> n
     return $ Type typeN


data Species = Species
             { speciesName :: Text
             , speciesUrl :: Text
             } deriving (Show, Eq)

data Sprites = Sprites
             { frontDefault :: Text
             } deriving (Show, Eq)

instance FromJSON Species where
  parseJSON (Object km) = do
      let mapText = toMapText km
          n       = M.lookup "name" mapText
          ur      = M.lookup "url" mapText
      case (n,ur) of
       (Just (String me),Just (String u)) ->
         return $ Species me u

data Pokemon = Pokemon
             { name :: Text
             , species :: Species
             , sprites :: Sprites
             , weight  :: Maybe Integer
             , height  :: Integer
             , typePokemon :: [Type]
             } deriving (Show, Eq)

instance FromJSON Sprites where
    parseJSON (Object km) = do
       let mapText = toMapText km
           f       = M.lookup "front_default" mapText
       case f of
        (Just (String fr)) ->
          return $ Sprites fr

instance FromJSON Pokemon where
     parseJSON (Object km) = do
       let mapText = toMapText km
           n       = M.lookup "name" mapText
           sp      = fromJust $ M.lookup "species" mapText
           spr     = fromJust $ M.lookup "sprites" mapText
           w       = fromJust $ M.lookup "weight" mapText
           t       = case (M.lookup "types" mapText) of
                      Nothing -> return V.empty
                      (Just (Array vector)) -> Monad.sequence $ do
                             obj <- vector
                             return $ ((parseJSON obj) :: Parser Type)
           h       = case (M.lookup "height" mapText) of
                      Nothing -> 0
                      (Just (Number s)) -> coefficient s

       sp'    <- (parseJSON sp) :: Parser Species
       spr'   <- (parseJSON spr) :: Parser Sprites
       vecTyp <- t :: Parser (Vector Type)
       case n of
        (Just (String nme)) -> return $ Pokemon {name = nme
                                                ,species = sp'
                                                ,sprites = spr'
                                                ,weight  = case w of
                                                            (Number s) -> Just $ coefficient s
                                                            _          -> Nothing
                                                ,typePokemon = V.toList vecTyp
                                                , height = h
                                                }
