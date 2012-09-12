{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual48 where
import Diagrams.Prelude

no = (circle 1 <> hrule 2 # rotateBy (1/8))
   # lw 0.2 # lc red
example = no <> image "static/phone.png" 1.5 1.5
