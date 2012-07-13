module Plugins.Core.Main (plugin, main) where

import Plugins.Types
import Data.Dynamic
import Control.Concurrent ( threadDelay,forkIO )

plugin = Plugin 
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  }

main :: Core -> IO ()
main core = do
  (a:_) <- loadPlugin core "Window"
  castMaybeDynamic a :: IO ()
  threadDelay 300000
