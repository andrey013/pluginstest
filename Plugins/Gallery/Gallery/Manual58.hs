{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual58 where

import Diagrams.Prelude

t = eqTriangle 5   # fc orange
s = square     3   # fc red
o = ellipseXY  2 3 # fc blue
c = circle     2   # fc green

d = centerX (t ||| s ||| o ||| c)

example = d
