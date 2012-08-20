module Plugins.Gloss.Main where

import Plugins.Types
import Graphics.Gloss

plugin :: Plugin
plugin = Plugin
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  }

main :: Core -> IO ()
main core = do
  (b:_) <- loadPlugin core "Application"
  let application = castMaybeDynamic b :: Application
  --initAppplication application
  initWindow' application

initWindow' :: Application -> IO ()
initWindow' app
  = play
        (InWindow
           "Hello World"     -- window title
        (400, 150)   -- window size
        (10, 10))    -- window position
    white            -- background color
    100
    emptyState
    picture          -- picture to display
    (processKey app)
    (processState app)

picture :: ApplicationState -> Picture
picture state
    = Translate (-170) (-20) -- shift to the middle of the window
    $ Scale 0.5 (angle state)      -- display it half the original size
    $ Text "Hello World"     -- text to display
