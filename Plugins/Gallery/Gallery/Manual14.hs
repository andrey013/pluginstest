{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual14 where

import Diagrams.Prelude

example = beside (r2 (20,30))
                 (circle 1 # fc orange)
                 (circle 1.5 # fc purple)
          # showOrigin
