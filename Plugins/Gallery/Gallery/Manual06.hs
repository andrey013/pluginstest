{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual06 where
import Diagrams.Prelude

example = square 1 ||| rect 0.3 0.5
      ||| eqTriangle 1
      ||| roundedRect  0.5 0.4 0.1
      ||| roundedRect  0.5 0.4 (-0.1)
      ||| roundedRect' 0.7 0.4 with { radiusTL = 0.2
                                    , radiusTR = -0.2
                                    , radiusBR = 0.1 }
