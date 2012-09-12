{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual37 where
import Diagrams.Prelude

ring :: Path R2
ring = circle 3 <> circle 2

example = stroke ring # fc purple # fillRule EvenOdd
