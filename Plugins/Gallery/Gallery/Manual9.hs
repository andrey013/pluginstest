{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual9 where
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points

example = star (StarSkip 3) (regPoly 13 1) # stroke
