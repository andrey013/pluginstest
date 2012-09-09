{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual38 where

import Diagrams.Prelude

dot = circle 1 # fc black
mkRow n = hcat' with {sep = 0.5} (replicate n dot)
mkTri n = decoratePath
            (fromOffsets (replicate (n-1) (2.5 *^ unitX))
               # rotateBy (1/6))
            (map mkRow [n, n-1 .. 1])
example = mkTri 5
