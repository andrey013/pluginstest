module Plugins.Core.Main (plugin, main) where

import Plugins.Types

plugin = Plugin 
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  }

main lp = do
  print "asd\nsdsa"
  a <- lp "Window"
  case a of
    Just p -> (p  :: IO ())
    Nothing -> return ()
