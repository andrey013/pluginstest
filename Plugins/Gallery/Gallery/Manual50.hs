{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual50 where

import Diagrams.Prelude

surround d = c === (c ||| d ||| c) # centerXY === c
  where c = circle 0.5

example = surround (square 1) ||| strutX 1
      ||| surround (pad 1.2 $ square 1)
