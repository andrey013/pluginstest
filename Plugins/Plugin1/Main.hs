module Plugins.Plugin1.Main (plugin) where

import Plugins.Types

plugin = Plugin 
  { pluginId = "Plugins.Plugin1"
  , extensionPoints = []
  , extentions = []
  , name = "1"
  , provider = "I"
  }

plugin1 :: (String -> String)
plugin1 a = "Hello, " ++ a ++ "!" ++ (show $ fibs !! 60)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
