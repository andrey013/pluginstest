{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual33 where
import Diagrams.Prelude

s = square 1 # fc yellow
example = hcat . map showOrigin
        $ zipWith alignY [-1, -0.8 .. 1] (repeat s)
