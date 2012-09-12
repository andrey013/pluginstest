{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual54 where
import Diagrams.Prelude

import Data.Maybe (fromMaybe)

root   = circle 1 # named "root"
leaves = centerXY
       . hcat' with {sep = 0.5}
       $ map (\c -> circle 1 # named c) "abcde"

parentToChild child
  = withName "root" $ \rb ->
    withName child  $ \cb ->
      atop (boundaryFrom rb unit_Y ~~ boundaryFrom cb unitY)

nodes  = root === strutY 2 === leaves

example = nodes # applyAll (map parentToChild "abcde")
