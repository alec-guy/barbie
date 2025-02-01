{-# LANGUAGE OverloadedStrings #-}
module Heart where


import Data.Vector as V
import Data.Text

h1 :: Text
h1 = "❤️"


h2 :: Text
h2 = "💚"

h3 = "🤍"

h4 = "🤎"

h5 = "💛"

h6 = "💙"

h7 = "🧡"

h8 = "🖤"

h9 = "ლ "

h10 = "ღ"

h11 = "❥"

h12 = "♡"

heartVector :: Vector Text
heartVector = V.fromList [h1
                         ,h2
                         ,h3
                         ,h4
                         ,h5
                         ,h6
                         ,h7
                         ,h8
                         ,h9
                         ,h10
                         ,h11
                         ,h12
                         , "💓"
                         , "💕"
                         , "💖"
                         , "💗"
                         ]
