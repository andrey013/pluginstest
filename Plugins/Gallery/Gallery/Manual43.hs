{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual43 where

import Diagrams.Prelude

example = square 3
        # fc green
        # lw 0.05
        # clipBy (square 3.2 # rotateBy (1/10))
