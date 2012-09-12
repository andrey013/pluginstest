{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual44 where
import Diagrams.Prelude

circles = (c ||| c) === (c ||| c) where c = circle 1 # fc fuchsia
example = circles # centerXY # view (p2 (-1,-1)) (r2 (1.3, 0.7))
