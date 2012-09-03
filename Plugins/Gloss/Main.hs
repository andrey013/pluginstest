{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gloss.Main where

import Plugins.Types
import qualified Graphics.Gloss as G
import Plugins.Gloss.DiagramsBackend
import Diagrams.Prelude

plugin :: Plugin
plugin = Plugin
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  } 

main :: Core -> IO ()
main core = do
  (b:_) <- loadPlugin core "Application"
  let application = castMaybeDynamic b :: Application GlossBackend
  --initAppplication application
  initWindow' application

initWindow' :: Application GlossBackend -> IO ()
initWindow' app
  = G.play (G.InWindow
          "Hello World"     -- window title
          (400, 150)   -- window size
          (10, 10))    -- window position
    G.white            -- background color
    100
    emptyState
    picture          -- picture to display
    (processKey app)
    (processState app)

picture :: ApplicationState GlossBackend -> G.Picture
picture state
    = G.Scale s s $ renderDia GlossBackend GlossOptions 
      (centerXY $ diagram state)
 where s = angle state 
