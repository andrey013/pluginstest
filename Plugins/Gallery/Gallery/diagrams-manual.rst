::

  example = circle 1

::

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

::

  example = circle 0.5 <> unitCircle

::

  example = unitCircle # scaleX 0.5 # rotateBy (1/6)

::

  example = arc (tau/4 :: Rad) (4 * tau / 7 :: Rad)

::

  example = square 1 ||| rect 0.3 0.5
        ||| eqTriangle 1
        ||| roundedRect  0.5 0.4 0.1
        ||| roundedRect  0.5 0.4 (-0.1)
        ||| roundedRect' 0.7 0.4 with { radiusTL = 0.2
                                      , radiusTR = -0.2
                                      , radiusBR = 0.1 }

::

  example = circle 1 ||| hrule 2 ||| circle 1

::

  poly1 = polygon with { polyType   = PolyRegular 13 5
                       , polyOrient = OrientV }
  poly2 = polygon with { polyType   = PolyPolar (repeat (1/40 :: CircleFrac))
                                                (take 40 $ cycle [2,7,4,6]) }
  example = (poly1 ||| strutX 1 ||| poly2) # lw 0.05

::

  example = star (StarSkip 3) (regPoly 13 1) # stroke

::

  example = stroke (star (StarSkip 2) (regPoly 8 1))
        ||| strutX 1
        ||| stroke (star (StarSkip 3) (regPoly 8 1))

::

  funs          = map (flip (^)) [2..6]
  visualize f	  = stroke' with { vertexNames = [[0 .. 6 :: Int]] }
                      (regPoly 7 1)
                    # lw 0
                    # showLabels
                    # fontSize 0.6
               <> star (StarFun f) (regPoly 7 1)
                    # stroke # lw 0.05 # lc red
  example       = centerXY . hcat' with {sep = 0.5} $ map visualize funs

::

  example = circle 1 `atop` square (sqrt 2)

::

  example = mconcat [ circle 0.1 # fc green
                    , eqTriangle 1 # scale 0.4 # fc yellow
                    , square 1 # fc blue
                    , circle 1 # fc red
                    ]

::

  example = beside (r2 (20,30))
                   (circle 1 # fc orange)
                   (circle 1.5 # fc purple)
            # showOrigin

::

  example = beside (r2 (20,30))
                   (circle 1   # fc orange # align (r2 (20,30)))
                   (circle 1.5 # fc purple)
            # showOrigin

::

  d1 = circle 1 # fc red
  d2 = square 1 # fc blue
  example = (d1 ||| d2) ||| strutX 3 ||| ( d1
                                           ===
                                           d2  )

::

  example = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat dot))
    where dot       = circle 0.2 # fc black
          mkPoint x = p2 (x,x^2)

::

  example = cat (r2 (2,-1)) (map p [3..8]) # showOrigin
    where p n = regPoly n 1 # lw 0.03

::

  example = cat' (r2 (2,-1)) with { catMethod = Distrib, sep = 2 } (map p [3..8])
    where p n = regPoly n 1 # lw 0.03
                            # scale (1 + fromIntegral n/4)
                            # showOrigin

::

  c        = circle 1 # lw 0.03
  dirs     = iterate (rotateBy (1/7)) unitX
  cdirs    = zip dirs (replicate 7 c)
  example1 = appends c cdirs
  example2 = foldl (\a (v,b) -> beside v a b) c cdirs
  example  = example1 ||| strutX 3 ||| example2

::

  example = circle 0.2 # lc purple # fc yellow

::

  import Data.Colour (withOpacity)
 
  colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
  example = hcat' with { catMethod = Distrib, sep = 1 }
                  (zipWith fcA colors (repeat (circle 1)))

::

  s c     = square 1 # fc c
  reds    = (s darkred ||| s red) === (s pink ||| s indianred)
  example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

::

  t = regPoly 3 1
 
  example = t ||| t # bg orange

::

  example = (square 1
        ||| square 1 # scale 2
        ||| circle 1 # scaleX 3)   # lw 0.03

::

  example = (square 1
        ||| square 1 # freeze # scale 2
        ||| circle 1 # freeze # scaleX 3)  # lw 0.03

::

  path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1
  example = centerXY . vcat' with { sep = 0.1 }
            $ map (path #)
              [ lineCap LineCapButt   . lineJoin LineJoinMiter
              , lineCap LineCapRound  . lineJoin LineJoinRound
              , lineCap LineCapSquare . lineJoin LineJoinBevel
              , dashing [0.1,0.2,0.3,0.1] 0
              ]

::

  eff = text "F" <> square 1 # lw 0
  rs  = map rotateBy [1/7, 2/7 .. 6/7]
  example = hcat . map (eff #) $ rs

::

  eff = text "F" <> square 1 # lw 0
  ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
        ,     scale (-1), scaleX (-1), scaleY (-1)
        ]
 
  example = hcat . map (eff #) $ ts

::

  eff = text "F" <> square 1 # lw 0
  example = eff
         <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff

::

  eff = text "F" <> square 1 # lw 0
  example = (scaleX 2 `under` rotation (-1/8 :: CircleFrac)) eff

::

  s = square 1 # fc yellow
  x |-| y = x ||| strutX 0.5 ||| y
  example =  (s # showOrigin)
         |-| (s # alignT  # showOrigin)
         |-| (s # alignBR # showOrigin)

::

  s = square 1 # fc yellow
  example = hcat . map showOrigin
          $ zipWith alignY [-1, -0.8 .. 1] (repeat s)

::

  illustrateBezier c1 c2 x2
      =  endpt
      <> endpt  # translate x2
      <> ctrlpt # translate c1
      <> ctrlpt # translate c2
      <> l1
      <> l2
      <> fromSegments [bezier3 c1 c2 x2]
    where
      dashed  = dashing [0.1,0.1] 0
      endpt   = circle 0.05 # fc red  # lw 0
      ctrlpt  = circle 0.05 # fc blue # lw 0
      l1      = fromOffsets [c1] # dashed
      l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed
 
  x2      = r2 (3,-1) :: R2         -- endpoint
  [c1,c2] = map r2 [(1,2), (3,0)]   -- control points
 
  example = illustrateBezier c1 c2 x2

::

  spike :: Trail R2
  spike = fromOffsets . map r2 $ [(1,3), (1,-3)]
 
  burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike
 
  example = strokeT burst # fc yellow # lw 0.1 # lc orange

::

  spike :: Trail R2
  spike = fromOffsets . map r2 $ [(1,3), (1,-3)]
 
  burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike
 
  colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]
 
  example = lw 0.1
          . mconcat
          . zipWith lc colors
          . map stroke . explodeTrail origin
          $ burst

::

  ring :: Path R2
  ring = circle 3 <> circle 2
 
  example = stroke ring # fc purple # fillRule EvenOdd

::

  dot = circle 1 # fc black
  mkRow n = hcat' with {sep = 0.5} (replicate n dot)
  mkTri n = decoratePath
              (fromOffsets (replicate (n-1) (2.5 *^ unitX))
                 # rotateBy (1/6))
              (map mkRow [n, n-1 .. 1])
  example = mkTri 5

::

  s = square 2  -- a squarish thing.
 
  blueSquares = decoratePath s {- 1 -}
                  (replicate 4 (s {- 2 -} # scale 0.5) # fc blue)
  paths       = lc purple . stroke $ star (StarSkip 2) s {- 3 -}
  aster       = centerXY . lc green . strokeT
              . mconcat . take 5 . iterate (rotateBy (1/5))
              $ s {- 4 -}
  example = (blueSquares <> aster <> paths) # lw 0.05

::

  ts = mconcat . take 3 . iterate (rotateBy (1/9)) $ eqTriangle 1
  example = (ts ||| stroke ts ||| strokeT ts ||| fromVertices ts) # fc red

::

  pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
  dot = circle 0.2 # fc blue # lw 0
  mkPath closed = position (zip pts (repeat dot))
               <> cubicSpline closed pts # lw 0.05
  example = mkPath False ||| strutX 2 ||| mkPath True

::

  loopyStar = fc red
            . mconcat . map (cubicSpline True)
            . pathVertices
            . star (StarSkip 3)
            $ regPoly 7 1
  example = loopyStar # fillRule EvenOdd
        ||| strutX 1
        ||| loopyStar # fillRule Winding

::

  example = square 3
          # fc green
          # lw 0.05
          # clipBy (square 3.2 # rotateBy (1/10))

::

  circles = (c ||| c) === (c ||| c) where c = circle 1 # fc fuchsia
  example = circles # centerXY # view (p2 (-1,-1)) (r2 (1.3, 0.7))

::

  example = text "Hello world!" <> rect 8 1

::

  pt = circle 0.1 # fc red
 
  t1 = pt <> topLeftText         "top left"   <> rect 8 1
  t2 = pt <> baselineText        "baseline"   <> rect 8 1
  t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1
 
  d1 =/= d2 = d1 === strutY 2 === d2
  example = t1 =/= t2 =/= t3

::

  text' s t = text t # fontSize s <> strutY (s * 1.3)
  example = centerXY $
        text' 10 "Hello" # italic
    === text' 5 "there"  # bold # font "freeserif"
    === text' 3 "world"  # fc green

::

  no = (circle 1 <> hrule 2 # rotateBy (1/8))
     # lw 0.2 # lc red
  example = no <> image "static/phone.png" 1.5 1.5

::

  example = circle 1 ||| strutX 2 ||| square 2

::

  surround d = c === (c ||| d ||| c) # centerXY === c
    where c = circle 0.5
 
  example = surround (square 1) ||| strutX 1
        ||| surround (pad 1.2 $ square 1)

::

  surround d = c === (c ||| d ||| c) # centerXY === c
    where c = circle 0.5
 
  p = strokeT (square 1)
 
  example = surround (pad 1.2 $ p # showOrigin) ||| strutX 1
        ||| surround (pad 1.2 $ p # centerXY # showOrigin)

::

  example = hcat [ square 2
                 , circle 1 # withEnvelope (square 3 :: D R2)
                 , square 2
                 , text "hi" <> phantom (circle 2 :: D R2)
                 ]

::

  data Foo = Baz | Bar | Wibble
    deriving (Typeable, Eq, Ord, Show)
 
  instance IsName Foo
 
  connect n1 n2
    = withName n1 $ \b1 ->
      withName n2 $ \b2 ->
        atop ((location b1 ~~ location b2) # lc red # lw 0.03)
 
  example = (square 3 # named Baz ||| circle 2.3 # named Bar)
          # connect Baz Bar

::

  import Data.Maybe (fromMaybe)
 
  root   = circle 1 # named "root"
  leaves = centerXY
         . hcat' with {sep = 0.5}
         $ map (\c -> circle 1 # named c) "abcde"
 
  parentToChild child
    = withName "root" $ \rb ->
      withName child  $ \cb ->
        atop (   fromMaybe origin (traceP (location rb) unitY rb)
              ~~ fromMaybe origin (traceP (location cb) unit_Y cb))
 
  nodes  = root === strutY 2 === leaves
 
  example = nodes # applyAll (map parentToChild "abcde")

::

  data Corner = NW | NE | SW | SE
    deriving (Typeable, Eq, Ord, Show)
  instance IsName Corner
 
  connect n1 n2
    = withName n1 $ \b1 ->
      withName n2 $ \b2 ->
        atop ((location b1 ~~ location b2) # lc red # lw 0.03)
 
  squares =  (s # named NW ||| s # named NE)
         === (s # named SW ||| s # named SE)
    where s = square 1
 
  d = hcat' with {sep = 0.5} (zipWith (|>) [0::Int ..] (replicate 5 squares))
 
  pairs :: [(Name, Name)]
  pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
          , ((1::Int) .> SE, (4::Int) .> NE)
          , ((3::Int) .> NW, (3::Int) .> SE)
          , ((0::Int) .> SE, (1::Int) .> NW)
          ]
 
  example = d # applyAll (map (uncurry connect) pairs)

::

  c :: Diagram Cairo R2
  c = circle 5 # scaleX 2 # rotateBy (1/14) # lw 0.03
 
  -- Generated by fair dice roll, guaranteed to be random
  points = map p2 $
           [ (0.8936218079179525,6.501173563301563)
           , (0.33932828810065985,9.06458375044167)
           , (2.12546952534467,4.603130561299622)
           , (-8.036711369641125,6.741718165576458)
           , (-9.636495308950543,-8.960315063595772)
           , (-5.125008672475815,-4.196763141080737)
           , (-8.740284494124353,-1.748269759118557)
           , (-2.7303729625418782,-9.902752498164773)
           , (-1.6317121405154467,-6.026127282530069)
           , (-3.363167801871896,7.5571909081190825)
           , (5.109759075567126,-5.433154460042715)
           , (8.492015791125596,-9.813023637980223)
           , (7.762080919928849,8.340037921443582)
           , (-6.8589746952056885,3.9604472182691097)
           , (-0.6083773449063301,-3.7738202372565866)
           , (1.3444943726062775,1.1363744735717773)
           , (0.13720748480409384,8.718934659846127)
           , (-5.091010760515928,-8.887260649353266)
           , (-5.828490639105439,-9.392594425007701)
           , (0.7190148020163178,1.4832069771364331)
           ]
 
  mkPoint p = (p, circle 0.3
            	  # lw 0
            	  # fc (case sample c p of
            	          Any True  -> red
            	          Any False -> blue
            	       )
              )
 
  example = c <> position (map mkPoint points)

::

  withCount = (# value (Sum 1))
 
  c :: QDiagram Cairo R2 (Sum Int)
  c = (   circle 5 # scaleX 2 # rotateBy (1/14) # withCount
       <> circle 2 # scaleX 5 # rotateBy (-4/14) # withCount
      )
      # lw 0.03
 
  -- Generated by fair dice roll, guaranteed to be random
  points = map p2 $
           [ (0.8936218079179525,6.501173563301563)
           , (0.33932828810065985,9.06458375044167)
           , (2.12546952534467,4.603130561299622)
           , (-8.036711369641125,6.741718165576458)
           , (-9.636495308950543,-8.960315063595772)
           , (-5.125008672475815,-4.196763141080737)
           , (-8.740284494124353,-1.748269759118557)
           , (-2.7303729625418782,-9.902752498164773)
           , (-1.6317121405154467,-6.026127282530069)
           , (-3.363167801871896,7.5571909081190825)
           , (5.109759075567126,-5.433154460042715)
           , (8.492015791125596,-9.813023637980223)
           , (7.762080919928849,8.340037921443582)
           , (-6.8589746952056885,3.9604472182691097)
           , (-0.6083773449063301,-3.7738202372565866)
           , (1.3444943726062775,1.1363744735717773)
           , (0.13720748480409384,8.718934659846127)
           , (-5.091010760515928,-8.887260649353266)
           , (-5.828490639105439,-9.392594425007701)
           , (0.7190148020163178,1.4832069771364331)
           ]
 
  mkPoint p = (p, circle (case sample c p of
                            Sum n  -> 2 * fromIntegral n / 5 + 1/5)
                  # fc black
              )
 
  example = c # clearValue <> position (map mkPoint points)

::

  t = eqTriangle 5   # fc orange
  s = square     3   # fc red
  o = ellipseXY  2 3 # fc blue
  c = circle     2   # fc green
 
  d = centerX (t ||| s ||| o ||| c)
 
  example = d

::

  t = eqTriangle 5   # fc orange
  s = square     3   # fc red
  o = ellipseXY  2 3 # fc blue
  c = circle     2   # fc green
 
  ds = centerX ([t] ||| [s] ||| [o] ||| [c])
  d' = mconcat $ zipWith translateY [0.5, -0.6, 0, 0.4] ds
 
  example = d'
