{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual29 where
import Diagrams.Prelude

eff = text "F" <> square 1 # lw 0
ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
      ,     scale (-1), scaleX (-1), scaleY (-1)
      ]

example = hcat . map (eff #) $ ts
