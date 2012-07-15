module Plugins.GLFW.Main (plugin, window) where

import Plugins.Types
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever,when )
import Data.Version

plugin = Plugin 
  { extentions = [Extension "Window" ["window"]]
  , name = "1"
  }

window = Window 
  { initWindow = initWindow'
  , close = closeWindow
  , frameDone = frameDone'
  , windowCloseEvent = windowClose
  }

initWindow' :: IO ()
initWindow' = do
  True <- GLFW.initialize
  let dspOpts = GLFW.defaultDisplayOptions
                  { GLFW.displayOptions_width = 1024
                  , GLFW.displayOptions_height = 768
                  , GLFW.displayOptions_numRedBits = 8
                  , GLFW.displayOptions_numGreenBits = 8
                  , GLFW.displayOptions_numBlueBits = 8
                  , GLFW.displayOptions_numAlphaBits = 8
                  , GLFW.displayOptions_numDepthBits = 1
                  , GLFW.displayOptions_displayMode = GLFW.Window
                  }
  True <- GLFW.openWindow dspOpts
  return ()

frameDone' :: IO ()
frameDone' = GLFW.swapBuffers

closeWindow :: IO ()
closeWindow = do
  GLFW.closeWindow
  GLFW.terminate

windowClose :: (IO ()) -> IO ()
windowClose fire = GLFW.setWindowCloseCallback $ fire >> return False
