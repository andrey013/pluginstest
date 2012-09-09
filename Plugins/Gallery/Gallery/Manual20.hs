{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual20 where

import Diagrams.Prelude

c        = circle 1 # lw 0.03
dirs     = iterate (rotateBy (1/7)) unitX
cdirs    = zip dirs (replicate 7 c)
example1 = appends c cdirs
example2 = foldl (\a (v,b) -> beside v a b) c cdirs
example  = example1 ||| strutX 3 ||| example2
