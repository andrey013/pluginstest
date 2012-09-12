{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual22 where
import Diagrams.Prelude

import Data.Colour (withOpacity)

colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
example = hcat' with { catMethod = Distrib, sep = 1 }
                (zipWith fcA colors (repeat (circle 1)))
