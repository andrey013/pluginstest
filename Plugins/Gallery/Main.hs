module Plugins.Gallery.Main (plugin, application) where

import Plugins.Types
import Plugins.Gloss.DiagramsBackend
import Plugins.Gallery.Gallery.FibCalls as F
import Plugins.Gallery.Gallery.Gray as Gr
-- import Plugins.Gallery.Gallery.Hasse as Ha
import Plugins.Gallery.Gallery.Hilbert as Hi
import Plugins.Gallery.Gallery.Mandelbrot as M
import Plugins.Gallery.Gallery.Paradox as Pa
import Plugins.Gallery.Gallery.Pentaflake as Pe
-- import Plugins.Gallery.Gallery.Quasifuchsian as F
-- import Plugins.Gallery.Gallery.Sierpinski as F
-- import Plugins.Gallery.Gallery.Star as F
-- import Plugins.Gallery.Gallery.TriangularNumbers as F
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

processState' :: Float -> ApplicationState GlossBackend  -> ApplicationState GlossBackend
processState' a st | angle st == 0 =
  st{diagram = (F.example ||| Gr.example ||| Hi.example
            === M.example ||| Pa.example ||| Pe.example :: Diagram GlossBackend R2)
               , angle = 0.1}
                   | otherwise     = st{angle = angle st + (10 * delta st * realToFrac a)}

processKey' ::  Event -> ApplicationState a  -> ApplicationState a
processKey' _ = id

-- fibs ::  [Integer]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
