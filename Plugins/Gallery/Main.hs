{-# LANGUAGE NoMonomorphismRestriction #-}
module Plugins.Gallery.Main (plugin, application) where

import Plugins.Types
import Plugins.Gloss.DiagramsBackend
import qualified Plugins.Gallery.Gallery.FibCalls as F
import qualified Plugins.Gallery.Gallery.Gray as Gr
-- import Plugins.Gallery.Gallery.Hasse as Ha
import qualified Plugins.Gallery.Gallery.Hilbert as Hi
import qualified Plugins.Gallery.Gallery.Mandelbrot as M
import qualified Plugins.Gallery.Gallery.Paradox as Pa
import qualified Plugins.Gallery.Gallery.Pentaflake as Pe
import qualified Plugins.Gallery.Gallery.Quasifuchsian as Q
import qualified Plugins.Gallery.Gallery.Sierpinski as Si
import qualified Plugins.Gallery.Gallery.Star as St
import qualified Plugins.Gallery.Gallery.TriangularNumbers as T
import Diagrams.Prelude

plugin :: Plugin
plugin = Plugin 
  { extentions = [Extension "Application" ["application"]]
  , name = "1"
  }

application ::  Application GlossBackend
application = Application
  { processState = processState'
  , processKey   = processKey'
  }

ts = mconcat . take 3 . iterate (rotateBy (1/9)) $ eqTriangle 1
exampleL = (ts ||| stroke ts ||| strokeT ts ||| fromVertices ts) # fc red

exampleM = mconcat [ circle 0.1 # fc green
                  , eqTriangle 1 # scale 0.4 # fc yellow
                  , square 1 # fc blue
                  , circle 1 # fc red
                  ]

s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)
exampleN = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

processState' :: Float -> ApplicationState GlossBackend  -> ApplicationState GlossBackend
processState' a st | angle st == 0 =
  st{diagram = (-- F.example ||| Gr.example ||| Hi.example 
                (eqTriangle 3 ||| exampleL)
                -- === (exampleM  ||| exampleN)
                -- M.example ||| Pa.example ||| Pe.example
                -- Q.example ||| Si.example ||| St.example
                -- T.example
             :: Diagram GlossBackend R2)
               , angle = 100}
                   | otherwise     = st{angle = angle st + (1 * delta st * realToFrac a)}

processKey' ::  Event -> ApplicationState a  -> ApplicationState a
processKey' _ = id

-- fibs ::  [Integer]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
