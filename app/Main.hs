{-# LANGUAGE OverloadedStrings #-}

module Main where

import Discord
import DiscordOpts
import Control.Monad (void)
import Data.Text
import Data.Text.IO
import Commands


getCatToken :: IO Text
getCatToken = Data.Text.IO.readFile "apikeys/catToken"

getDiscordToken :: IO Text
getDiscordToken = Data.Text.IO.readFile "apikeys/discordToken"

main :: IO ()
main = do
   cattoken <- getCatToken
   bottoken <- getDiscordToken
   result   <- runDiscord (myDiscordOpts bottoken cattoken [catimage,heart,joke, dogimage,foximage])
   Data.Text.IO.putStrLn result
