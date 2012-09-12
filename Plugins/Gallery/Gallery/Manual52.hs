{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual52 where
import Diagrams.Prelude

example = hcat [ square 2
               , circle 1 # withEnvelope (square 3 :: D R2)
               , square 2
               , text "hi" <> phantom (circle 2 :: D R2)
               ]
