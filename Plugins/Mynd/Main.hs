module Plugins.Mynd.Main (plugin, application) where

import Plugins.Types
import Graphics.Gloss.Interface.Pure.Game

plugin :: Plugin
plugin = Plugin 
  { extentions = [Extension "Application" ["application"]]
  , name = "1"
  }

application :: Application
application = Application
  { processState = processState'
  , processKey   = processKey'
  }

processState' :: Float -> ApplicationState -> ApplicationState
processState' a st = st{angle = angle st + (delta st * realToFrac a)}

processKey' :: Event -> ApplicationState -> ApplicationState
processKey' _ = id

fibs ::  [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
