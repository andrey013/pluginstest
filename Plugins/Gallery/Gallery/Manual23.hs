{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual23 where

import Diagrams.Prelude

s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)
example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds
