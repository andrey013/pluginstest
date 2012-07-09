module Plugins.Plugin2.Main (plugin) where

import qualified Plugins.Plugin1.Main
import Plugins.Types

plugin = Plugin 
  { pluginId = "Plugins.Plugin2"
  , extensionPoints = []
  , extentions = []
  , name = "2"
  , provider = "I"
  }

plugin1 :: (String -> String)
plugin1 a = "Hello, " ++ a ++ "!" ++ (show $ fibs !! 60)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
