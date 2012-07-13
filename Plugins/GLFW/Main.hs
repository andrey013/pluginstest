module Plugins.GLFW.Main (plugin, initWindow) where

import Plugins.Types
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever,when )
import System.Exit ( exitSuccess )
import Data.Version

plugin = Plugin 
  { extentions = [Extension "Window" ["initWindow"]]
  , name = "1"
  }

initWindow = do
  True <- GLFW.initialize
  let dspOpts = GLFW.defaultDisplayOptions
                  { GLFW.displayOptions_width = 1024
                  , GLFW.displayOptions_height = 768
                  , GLFW.displayOptions_numRedBits = 8
                  , GLFW.displayOptions_numGreenBits = 8
                  , GLFW.displayOptions_numBlueBits = 8
                  --, GLFW.displayOptions_numAlphaBits = 8
                  --, GLFW.displayOptions_numDepthBits = 1
                  , GLFW.displayOptions_displayMode = GLFW.Window
                  }
  --print =<< GLFW.openGLProfile
  --print =<< GLFW.getGlfwVersion
  --print =<< GLFW.getGlVersion
  --print "Trying to open the window"
  True <- GLFW.openWindow dspOpts
  --print result
  return ()
