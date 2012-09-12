{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual40 where
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points

ts = mconcat . take 3 . iterate (rotateBy (1/9)) $ eqTriangle 1
example = (ts ||| stroke ts ||| strokeT ts ||| fromVertices ts) # fc red
