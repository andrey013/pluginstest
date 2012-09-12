{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual17 where
import Diagrams.Prelude

example = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat dot))
  where dot       = circle 0.2 # fc black
        mkPoint x = p2 (x,x^2)
