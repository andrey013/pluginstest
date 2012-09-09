{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual28 where

import Diagrams.Prelude

eff = text "F" <> square 1 # lw 0
rs  = map rotateBy [1/7, 2/7 .. 6/7]
example = hcat . map (eff #) $ rs
