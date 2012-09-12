{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual59 where
import Diagrams.Prelude

t = eqTriangle 5   # fc orange
s = square     3   # fc red
o = ellipseXY  2 3 # fc blue
c = circle     2   # fc green

ds = centerX ([t] ||| [s] ||| [o] ||| [c])
d' = mconcat $ zipWith translateY [0.5, -0.6, 0, 0.4] ds

example = d'
