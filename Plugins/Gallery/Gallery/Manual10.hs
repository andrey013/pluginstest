{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual10 where

import Diagrams.Prelude

example = stroke (star (StarSkip 2) (regPoly 8 1))
      ||| strutX 1
      ||| stroke (star (StarSkip 3) (regPoly 8 1))
