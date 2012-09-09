{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual16 where

import Diagrams.Prelude

d1 = circle 1 # fc red
d2 = square 1 # fc blue
example = (d1 ||| d2) ||| strutX 3 ||| ( d1
                                         ===
                                         d2  )
