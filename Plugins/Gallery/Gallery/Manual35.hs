{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual35 where

import Diagrams.Prelude

spike :: Trail R2
spike = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike

example = strokeT burst # fc yellow # lw 0.1 # lc orange
