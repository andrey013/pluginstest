module Plugins.Plugin1.Main (plugin) where

import Plugins.Types

plugin = Plugin 
  { extentions = [Extension "2" ["1","2"]]
  , name = "1"
  }

plugin1 :: (String -> String)
plugin1 a = "Hello, " ++ a ++ "!" ++ (show $ fibs !! 60)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
