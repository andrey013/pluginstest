{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual34 where
import Diagrams.Prelude

illustrateBezier c1 c2 x2
    =  endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bezier3 c1 c2 x2]
  where
    dashed  = dashing [0.1,0.1] 0
    endpt   = circle 0.05 # fc red  # lw 0
    ctrlpt  = circle 0.05 # fc blue # lw 0
    l1      = fromOffsets [c1] # dashed
    l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

x2      = r2 (3,-1) :: R2         -- endpoint
[c1,c2] = map r2 [(1,2), (3,0)]   -- control points

example = illustrateBezier c1 c2 x2
