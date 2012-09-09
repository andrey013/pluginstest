{-# LANGUAGE NoMonomorphismRestriction #-}

module Plugins.Gallery.Gallery.Manual53 where

import Diagrams.Prelude

data Foo = Baz | Bar | Wibble
  deriving (Typeable, Eq, Ord, Show)

instance IsName Foo

connect n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lw 0.03)

example = (square 3 # named Baz ||| circle 2.3 # named Bar)
        # connect Baz Bar
