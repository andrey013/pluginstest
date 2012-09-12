{-# LANGUAGE NoMonomorphismRestriction
           #-}
module Plugins.Gallery.Gallery.Manual2 where
import Diagrams.Prelude

illustrateEnvelope v d
  = mconcat
    [ origin ~~ (origin .+^ v)
      # lc black # lw 0.03
    , polygon with { polyType   = PolyRegular 3 0.1
                   , polyOrient = OrientTo (negateV v)
                   }
      # fc black
      # translate v
    , origin ~~ b
      # lc green # lw 0.05
    , p1 ~~ p2
      # lc red # lw 0.02
    ]
    where
      b  = envelopeP v d
      v' = normalized v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

d1 :: Path R2
d1 = circle 1

d2 :: Path R2
d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)

example = (stroke d1 # showOrigin <> illustrateEnvelope (r2 (-0.5,0.3)) d1)
      ||| (stroke d2 # showOrigin <> illustrateEnvelope (r2 (0.5, 0.2)) d2)
