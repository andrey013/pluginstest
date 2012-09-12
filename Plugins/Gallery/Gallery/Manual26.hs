{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual26 where
import Diagrams.Prelude

example = (square 1
      ||| square 1 # freeze # scale 2
      ||| circle 1 # freeze # scaleX 3)  # lw 0.03
