module Plugins.Gloss.Main where

import Plugins.Types
import Graphics.Gloss

plugin = Plugin
  { extentions = [Extension "Window" ["window"], Extension "Core" ["main"]]
  , name = "1"
  }

window = Window
  { initWindow = initWindow'
  , close = return ()
  , frameDone = return ()
  , windowCloseEvent = \_ -> return ()
  }

main :: Core -> IO ()
main core = do

  (a:_) <- loadPlugin core "Window"
  let window = castMaybeDynamic a :: Window

  (b:_) <- loadPlugin core "Application"
  let application = castMaybeDynamic b :: Application
  --initAppplication application
  initWindow window

initWindow'
  = display
        (InWindow
           "Hello World"     -- window title
        (400, 150)   -- window size
        (10, 10))    -- window position
    white            -- background color
    picture          -- picture to display

picture
    = Translate (-170) (-20) -- shift the text to the middle of the window
    $ Scale 0.5 0.5      -- display it half the original size
    $ Text "Hello World"     -- text to display
