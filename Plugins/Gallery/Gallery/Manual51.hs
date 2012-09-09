{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual51 where

import Diagrams.Prelude

surround d = c === (c ||| d ||| c) # centerXY === c
  where c = circle 0.5

p = strokeT (square 1)

example = surround (pad 1.2 $ p # showOrigin) ||| strutX 1
      ||| surround (pad 1.2 $ p # centerXY # showOrigin)
