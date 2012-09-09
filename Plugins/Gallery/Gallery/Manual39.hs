{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual39 where

import Diagrams.Prelude

s = square 2  -- a squarish thing.

blueSquares = decoratePath s {- 1 -}
                (replicate 4 (s {- 2 -} # scale 0.5) # fc blue)
paths       = lc purple . stroke $ star (StarSkip 2) s {- 3 -}
aster       = centerXY . lc green . strokeT
            . mconcat . take 5 . iterate (rotateBy (1/5))
            $ s {- 4 -}
example = (blueSquares <> aster <> paths) # lw 0.05
