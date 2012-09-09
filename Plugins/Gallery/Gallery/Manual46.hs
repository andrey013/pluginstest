{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual46 where

import Diagrams.Prelude

pt = circle 0.1 # fc red

t1 = pt <> topLeftText         "top left"   <> rect 8 1
t2 = pt <> baselineText        "baseline"   <> rect 8 1
t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1

d1 =/= d2 = d1 === strutY 2 === d2
example = t1 =/= t2 =/= t3
