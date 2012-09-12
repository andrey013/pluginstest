{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual41 where
import Diagrams.Prelude

pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
dot = circle 0.2 # fc blue # lw 0
mkPath closed = position (zip pts (repeat dot))
             <> cubicSpline closed pts # lw 0.05
example = mkPath False ||| strutX 2 ||| mkPath True
