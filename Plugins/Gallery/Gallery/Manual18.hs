{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual18 where
import Diagrams.Prelude

example = cat (r2 (2,-1)) (map p [3..8]) # showOrigin
  where p n = regPoly n 1 # lw 0.03
