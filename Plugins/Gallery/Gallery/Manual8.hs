{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual8 where

import Diagrams.Prelude

poly1 = polygon with { polyType   = PolyRegular 13 5
                     , polyOrient = OrientV }
poly2 = polygon with { polyType   = PolyPolar (repeat (1/40 :: CircleFrac))
                                              (take 40 $ cycle [2,7,4,6]) }
example = (poly1 ||| strutX 1 ||| poly2) # lw 0.05
