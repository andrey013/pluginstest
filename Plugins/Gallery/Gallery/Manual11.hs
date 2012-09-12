{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual11 where
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points

funs          = map (flip (^)) [2..6]
visualize f	  = stroke' with { vertexNames = [[0 .. 6 :: Int]] }
                    (regPoly 7 1)
                  # lw 0
                  # showLabels
                  # fontSize 0.6
             <> star (StarFun f) (regPoly 7 1)
                  # stroke # lw 0.05 # lc red
example       = centerXY . hcat' with {sep = 0.5} $ map visualize funs
