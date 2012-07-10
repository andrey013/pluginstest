module Plugins.Plugin2.Main (plugin) where

import qualified Plugins.Plugin1.Main
import Plugins.Types

plugin = Plugin 
  { extentions = [Extension "1" ["3","4"], Extension "2" ["7","8"]]
  , name = "2"
  }

plugin1 :: (String -> String)
plugin1 a = "Hello, " ++ a ++ "!" ++ (show $ fibs !! 60)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
