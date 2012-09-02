{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gloss.Main where

import Plugins.Types
import qualified Graphics.Gloss as G
import Plugins.Gloss.DiagramsBackend
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree

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

picture :: ApplicationState -> G.Picture
picture state
    = renderDia GlossBackend GlossOptions 
      (centerXY $
          example
      ||| example1
      ||| example2)

fibCalls :: Integer -> BTree Integer
fibCalls 0 = leaf 0
fibCalls 1 = leaf 1
fibCalls n = BNode n (fibCalls (n-1)) (fibCalls (n-2))

Just t = uniqueXLayout 2 2 (fibCalls 5)

example = pad 1.1 . lw 0.03 . centerXY
        $ renderTree 
            (\n -> (text ("fib " ++ show n)
                    -- <> roundedRect 3 1.3 0.3 # fc white)
                    <> circle 1)
            )
            (~~) t

example1 :: Diagram GlossBackend R2
example1 = circle 0.5 <> unitCircle

example2 :: Diagram GlossBackend R2
example2 = square 1 ||| rect 0.3 0.5
       ||| eqTriangle 1
       ||| roundedRect  0.5 0.4 0.1
       ||| roundedRect  0.5 0.4 (-0.1)
       ||| roundedRect' 0.7 0.4 with { radiusTL = 0.2
                                     , radiusTR = -0.2
                                     , radiusBR = 0.1 }
