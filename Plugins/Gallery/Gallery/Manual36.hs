{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual36 where

import Diagrams.Prelude

spike :: Trail R2
spike = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

example = lw 0.1
        . mconcat
        . zipWith lc colors
        . map stroke . explodeTrail origin
        $ burst
