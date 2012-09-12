{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual47 where
import Diagrams.Prelude

text' s t = text t # fontSize s <> strutY (s * 1.3)
example = centerXY $
      text' 10 "Hello" # italic
  === text' 5 "there"  # bold # font "freeserif"
  === text' 3 "world"  # fc green
