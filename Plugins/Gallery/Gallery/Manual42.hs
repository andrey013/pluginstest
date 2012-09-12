{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual42 where
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points

loopyStar = fc red
          . mconcat . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ regPoly 7 1
example = loopyStar # fillRule EvenOdd
      ||| strutX 1
      ||| loopyStar # fillRule Winding
