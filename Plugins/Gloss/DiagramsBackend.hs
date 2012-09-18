{-# Language FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
           , ViewPatterns
           , StandaloneDeriving
  #-}
module Plugins.Gloss.DiagramsBackend where

-- from base
import Data.Typeable
import Control.Monad.State
import System.IO.Unsafe
import Data.Tuple

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Vector as G
import qualified Graphics.Gloss.Geometry.Angle as G
import qualified Graphics.Rendering.OpenGL as GL

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Path
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Text
import Diagrams.TwoD.Image
import Graphics.Rendering.Diagrams.Points

import Data.List.Split      (chunksOf)

import Plugins.Types

deriving instance Typeable Any

data GlossBackend = GlossBackend
    deriving (Show, Typeable)

data GlossRenderState =
  GlossRenderState{ currentLineColor  :: G.Color
                  , currentFillColor  :: G.Color
                  , currentLineWidth  :: Float
                  , currentLineCap    :: LineCap
                  , currentLineJoin   :: LineJoin
                  , currentFillRule   :: GL.TessWinding
                  , currentDashArray  :: [Float]
                  , currentDashOffset :: Float
                  , currentClip       :: [[G.Point]]
                  }

initialGlossRenderState = GlossRenderState
                            (G.makeColor 0 0 0 1)
                            (G.makeColor 1 1 1 0)
                            0.01
                            LineCapButt
                            LineJoinMiter
                            GL.TessWindingNonzero
                            []
                            0
                            []

type GlossRenderM a = State GlossRenderState a

instance Monoid (Render GlossBackend v) where
  mempty      = R $ return mempty
  mappend (R a) (R b) =
    R $ do
      p1 <- a
      p2 <- b
      return $ p1 `mappend` p2

instance HasLinearMap v => Backend GlossBackend v where
  data Render GlossBackend v = R (GlossRenderM G.Picture)
  type Result GlossBackend v = G.Picture
  data Options GlossBackend v = GlossOptions

  withStyle _ s _ (R p) = 
      R $ do
        mapM_ ($ s)
          [ changeLineColor
          , changeFillColor
          , changeLineWidth
          , changeLineCap
          , changeLineJoin
          , changeFillRule
          , changeDashing
          , changeClip
          ]
        p
  doRender _ _ (R p) = evalState p initialGlossRenderState

instance Renderable (Segment R2) GlossBackend where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) GlossBackend where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Path R2) GlossBackend where
  render _ = renderPath

instance Renderable Text GlossBackend where
  render _ = renderText

instance Renderable Image GlossBackend where
  render _ t = mempty

-- renderText :: Text -> G.Picture
renderText (Text tr _ str) = R . return $ G.text str

-- renderPath :: Path R2 -> G.Picture
renderPath (Path trs) =
  R $ do
    fc <- gets currentFillColor
    lc <- gets currentLineColor
    fr <- gets currentFillRule
    lw <- gets currentLineWidth
    lcap <- gets currentLineCap
    lj <- gets currentLineJoin
    darr <- gets currentDashArray
    doff <- gets currentDashOffset
    clip <- gets currentClip
    put initialGlossRenderState
    return $
      (G.Color fc $ G.Pictures $ map renderPolygon $ clippedPolygons (simplePolygons fr) clip)
      `mappend` (G.Color lc $ G.Pictures $ map renderPolygon $ clippedPolygons (linePolygons lw lcap lj) clip)
 where trails                  = map calcTrail trs
       simplePolygons fr       = tessRegion fr trails
       linePolygons lw lcap lj = tessRegion GL.TessWindingNonzero $ mconcat . map (calcLines lw lcap lj) $ trails
       clippedPolygons vis clip@(_:_) = tessRegion GL.TessWindingAbsGeqTwo (vis `mappend` clip)
       clippedPolygons vis _ = vis


renderPolygon :: [G.Point] -> G.Picture
renderPolygon = G.Polygon


{- Points calculation-}

calcLines :: Float -> LineCap -> LineJoin -> [G.Point] -> [[G.Point]]
calcLines lw lcap lj pp@(_:_:_) =
  map (calcLine lw) lines
 -- `mappend` G.line pp
  `mappend` (calcCap lw lcap $ swap $ head lines)
  `mappend` (calcCap lw lcap $ last lines)
  `mappend` map (calcJoin lj lw) joins
  `mappend` case G.magV distance > 0.0001 of
    True -> mempty
    False -> [calcJoin lj lw (c0, c1, c2)]
 where lines = zip  pp (tail pp)
       joins = zip3 pp (tail pp) (tail $ tail pp)
       c0         = pp !! (length pp - 2)
       (x1, y1) = last pp
       c1@(x2, y2) = head pp
       c2         = pp !! 1
       distance = (x1 - x2, y1 - y2)
calcLines _ _ _ _ = mempty

calcCap :: Float -> LineCap -> (G.Point, G.Point) -> [[G.Point]]
calcCap lwf lcap ((x1, y1), (x2, y2)) =
  -- G.Translate x2 y2 $ G.Rotate (-angle) 
  [cap]
 where cap = case lcap of
               LineCapButt   -> mempty
               LineCapRound  -> calcTrail (p2 (realToFrac $ x2 + c1, realToFrac $ y2 + c2),
                                          (arcT (Rad $ -tau/4) (Rad $ tau/4)) # scale (realToFrac lwf/2)
                                                                              #rotate (Rad $ realToFrac angle))
               LineCapSquare -> [ (x2 + c1, y2 + c2)
                                , (x2 - c1, y2 - c2)
                                , (x2 - c1 + n1, y2 - c2 + n2)
                                , (x2 + c1 + n1, y2 + c2 + n2)
                                ]
       vec   = (x2 - x1, y2 - y1)
       norm@(n1, n2)  = G.mulSV (lwf/2) . G.normaliseV $ vec
       v1@(c1, c2) = G.rotateV (-tau/4) norm
       angle = G.argV $ vec

calcJoin :: LineJoin -> Float -> (G.Point, G.Point, G.Point) -> [G.Point]
calcJoin lj lwf ((x1, y1), (x2, y2), (x3, y3)) =
  case lj of
    LineJoinMiter -> case (abs spikeLength) > (5 * lwf) of
                       True  -> bevel
                       False -> spike
    LineJoinRound -> -- G.Translate x2 y2 $ G.circleSolid (lwf/2)
      ((x2, y2):) $ calcTrail (p2 (realToFrac $ x2 + c3, realToFrac $ y2 + c4),
                                  (arcT (Rad $ realToFrac $ G.argV v2) (Rad $ realToFrac $ G.argV v1)) # scale (realToFrac lwf/2))
    LineJoinBevel -> bevel
 where vec1        = (x2 - x1, y2 - y1)
       vec2        = (x3 - x2, y3 - y2)
       norm1       = G.mulSV (lwf/2) . G.normaliseV $ vec1
       norm2       = G.mulSV (lwf/2) . G.normaliseV $ vec2
       side        = case (G.detV norm1 norm2) > 0 of
                       True  ->  1
                       False -> -1
       v1@(c1, c2)    = G.rotateV (side * (-tau/4)) norm1
       v2@(c3, c4)    = G.rotateV (side * (-tau/4)) norm2
       bevel       = [ (x2 + c1, y2 + c2)
                     , (x2 + c3, y2 + c4)
                     , (x2, y2)
                     ]
       angle       = (G.angleVV v1 v2) / 2
       spikeLength = (lwf/2) / cos(angle)
       (x4, y4)    = G.mulSV spikeLength $ G.unitVectorAtAngle $
                       (G.argV v1) + side * angle
       spike       = [ (x2 + c1, y2 + c2)
                     , (x2 + x4, y2 + y4)
                     , (x2 + c3, y2 + c4)
                     , (x2, y2)
                     ]

calcLine :: Float -> (G.Point, G.Point) -> [G.Point]
calcLine lw ((x1, y1), (x2, y2)) =
  [ (x1 + c1, y1 + c2)
  , (x1 - c1, y1 - c2)
  , (x2 - c1, y2 - c2)
  , (x2 + c1, y2 + c2)
  ]
 where vec    = (x2 - x1, y2 - y1)
       norm   = G.mulSV (lw/2) . G.normaliseV $ vec
       (c1, c2) = G.rotateV (tau/4) norm

tessRegion :: GL.TessWinding -> [[G.Point]] -> [[G.Point]]
tessRegion fr pp = renderSimplePolygon $ unsafePerformIO $
  GL.tessellate fr 0 (GL.Normal3 0 0 1)
  (\vv (GL.WeightedProperties (_,p) _ _ _) -> p) $
  GL.ComplexPolygon [GL.ComplexContour (map createVertex p) | p <- pp]
 where createVertex (x,y) =
         GL.AnnotatedVertex (GL.Vertex3 (realToFrac x) (realToFrac y) 0)
                            (0::Int)
       renderSimplePolygon (GL.SimplePolygon pp) =
         mconcat $ map renderSimplePrimitive pp
       renderSimplePrimitive (GL.Primitive GL.Polygon vv) =
         [map (\(GL.AnnotatedVertex (GL.Vertex3 x y _) _) -> (realToFrac x, realToFrac y)) vv]
       renderSimplePrimitive (GL.Primitive GL.TriangleFan vv) =
         [map (\(GL.AnnotatedVertex (GL.Vertex3 x y _) _) -> (realToFrac x, realToFrac y)) vv]
       renderSimplePrimitive (GL.Primitive GL.Triangles vv) =
         map (\[ (GL.AnnotatedVertex (GL.Vertex3 x0 y0 _) _)
               , (GL.AnnotatedVertex (GL.Vertex3 x1 y1 _) _)
               , (GL.AnnotatedVertex (GL.Vertex3 x2 y2 _) _)
               ]
               ->
               [ (realToFrac x0, realToFrac y0)
               , (realToFrac x1, realToFrac y1)
               , (realToFrac x2, realToFrac y2)
               ]) $ chunksOf 3 vv
       renderSimplePrimitive (GL.Primitive GL.TriangleStrip vv) =
         map (\( (GL.AnnotatedVertex (GL.Vertex3 x0 y0 _) _)
               , (GL.AnnotatedVertex (GL.Vertex3 x1 y1 _) _)
               , (GL.AnnotatedVertex (GL.Vertex3 x2 y2 _) _)
               )
               ->
               [ (realToFrac x0, realToFrac y0)
               , (realToFrac x1, realToFrac y1)
               , (realToFrac x2, realToFrac y2)
               ]) $ zip3 vv (tail vv) (tail $ tail vv)
       renderSimplePrimitive (GL.Primitive pm vv) = unsafePerformIO $ print pm >> return []

calcTrail :: (P2, Trail R2) -> [G.Point]
calcTrail (unp2 -> (x,y), Trail segs closed) =
  (mconcat segments)
  `mappend` 
  if closed
    && G.magV distance > 0.0001
    then
      initLine
    else
      mempty
 where segments = scanl calcSeg initLine segs
       initLine = [(realToFrac x, realToFrac y)]
       (x1, y1) = last initLine
       (x2, y2) = last . last $ segments
       distance = (x1 - x2, y1 - y2)

calcSeg :: [G.Point] -> Segment R2 -> [G.Point]
calcSeg lt (Linear (unr2 -> (x0,y0))) =
  [(x + realToFrac x0, y + realToFrac y0)]
 where (x, y) = last lt
calcSeg lt (Cubic  (unr2 -> (dx0,dy0)) (unr2 -> (dx1,dy1)) (unr2 -> (dx2,dy2))) =
  map point [step, 2 * step .. 1]
 where point t =
         let
           q1 = t*t*t*(-1) + t*t*3 + t*(-3) + 1
           q2 = t*t*t*3 + t*t*(-6) + t*3
           q3 = t*t*t*(-3) + t*t*3
           q4 = t*t*t
           qx = q1*x + q2*x0 + q3*x1 + q4*x2
           qy = q1*y + q2*y0 + q3*y1 + q4*y2
         in (qx, qy)
       (x2, y2) = (x + realToFrac dx2, y + realToFrac dy2)
       (x1, y1) = (x + realToFrac dx1, y + realToFrac dy1)
       (x0, y0) = (x + realToFrac dx0, y + realToFrac dy0)
       ( x,  y) = last lt
       step = 0.1


{- Style changes -}

changeLineColor :: Style v -> GlossRenderM ()
changeLineColor s =
  case lc of
    Just (r, g, b, a) -> modify (\s ->
      s{currentLineColor = G.makeColor (realToFrac r)
                                       (realToFrac g)
                                       (realToFrac b)
                                       (realToFrac a)
       })
    Nothing           -> return ()
 where lc = colorToRGBA <$> getLineColor <$> getAttr s

changeFillColor :: Style v -> GlossRenderM ()
changeFillColor s =
  case fc of
    Just (r, g, b, a) -> modify (\s ->
      s{currentFillColor = G.makeColor (realToFrac r)
                                       (realToFrac g)
                                       (realToFrac b)
                                       (realToFrac a)
       })
    Nothing           -> return ()
 where fc = colorToRGBA <$> getFillColor <$> getAttr s

changeLineWidth :: Style v -> GlossRenderM ()
changeLineWidth s =
  case lineWidth of
    Just a  -> modify (\s -> s{currentLineWidth = realToFrac a})
    Nothing -> return ()
 where lineWidth = getLineWidth <$> getAttr s

changeLineCap :: Style v -> GlossRenderM ()
changeLineCap s =
  case lineCap of
    Just a  -> modify (\s -> s{currentLineCap = a})
    Nothing -> return ()
 where lineCap = getLineCap <$> getAttr s

changeLineJoin:: Style v -> GlossRenderM ()
changeLineJoin s =
  case lineJoin of
    Just a  -> modify (\s -> s{currentLineJoin = a})
    Nothing -> return ()
 where lineJoin = getLineJoin <$> getAttr s

changeFillRule :: Style v -> GlossRenderM ()
changeFillRule s =
  case fillRule of
    Just Winding -> modify (\s ->
      s{currentFillRule = GL.TessWindingNonzero})
    Just EvenOdd -> modify (\s ->
      s{currentFillRule = GL.TessWindingOdd})
    Nothing      -> return ()
 where fillRule = getFillRule <$> getAttr s

changeDashing :: Style v -> GlossRenderM ()
changeDashing s =
  case dashing of
    Just (Dashing a o) -> modify (\s ->
      s{ currentDashArray  = map realToFrac a
       , currentDashOffset = realToFrac o
       })
    Nothing      -> return ()
 where dashing = getDashing <$> getAttr s

changeClip :: Style v -> GlossRenderM ()
changeClip s =
  case clip of
    Just ((Path trs):ps) -> modify (\s ->
      s{ currentClip = tessRegion GL.TessWindingNonzero $ map calcTrail trs
       })
    Just _       -> return ()
    Nothing      -> return ()
 where clip = getClip <$> getAttr s

