{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual32 where
import Diagrams.Prelude

s = square 1 # fc yellow
x |-| y = x ||| strutX 0.5 ||| y
example =  (s # showOrigin)
       |-| (s # alignT  # showOrigin)
       |-| (s # alignBR # showOrigin)
