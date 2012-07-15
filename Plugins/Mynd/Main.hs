module Plugins.Mynd.Main (plugin, application) where

import Plugins.Types

plugin = Plugin 
  { extentions = [Extension "Application" ["application"]]
  , name = "1"
  }

application = Application
  { processState = processState'
  }

processState' :: ApplicationState -> UpdateEvent -> ApplicationState
processState' st (UpdateState a) = st{angle = (angle st) + (delta st * realToFrac a / 1000)}

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
