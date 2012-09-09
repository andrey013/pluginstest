{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual31 where

import Diagrams.Prelude

eff = text "F" <> square 1 # lw 0
example = (scaleX 2 `under` rotation (-1/8 :: CircleFrac)) eff
