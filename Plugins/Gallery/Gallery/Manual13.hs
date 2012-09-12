{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual13 where
import Diagrams.Prelude

example = mconcat [ circle 0.1 # fc green
                  , eqTriangle 1 # scale 0.4 # fc yellow
                  , square 1 # fc blue
                  , circle 1 # fc red
                  ]
