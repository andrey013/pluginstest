{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual19 where

import Diagrams.Prelude

example = cat' (r2 (2,-1)) with { catMethod = Distrib, sep = 2 } (map p [3..8])
  where p n = regPoly n 1 # lw 0.03
                          # scale (1 + fromIntegral n/4)
                          # showOrigin
