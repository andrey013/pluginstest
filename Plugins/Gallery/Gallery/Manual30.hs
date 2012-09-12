{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual30 where
import Diagrams.Prelude

eff = text "F" <> square 1 # lw 0
example = eff
       <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff
