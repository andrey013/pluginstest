{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual25 where

import Diagrams.Prelude

example = (square 1
      ||| square 1 # scale 2
      ||| circle 1 # scaleX 3)   # lw 0.03
