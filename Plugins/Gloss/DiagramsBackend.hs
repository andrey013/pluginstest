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
import Diagrams.TwoD.Path (getClip)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Path (getFillRule, getClip)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Image

import Data.List.Split      (chunksOf)

import Plugins.Types

deriving instance Typeable Any

data GlossBackend = GlossBackend
    deriving (Show, Typeable)

data GlossRenderState =
  GlossRenderState{ currentLineColor :: G.Color
                  , currentFillColor :: G.Color
                  , currentLineWidth :: Double
                  , currentLineCap   :: LineCap
                  , currentLineJoin  :: LineJoin
                  , currentFillRule  :: GL.TessWinding
                  }

initialGlossRenderState = GlossRenderState
                            (G.makeColor 0 0 0 1)
                            (G.makeColor 1 1 1 0)
                            0.01
                            LineCapButt
                            LineJoinMiter
                            GL.TessWindingNonzero

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
          ]
        p
  doRender _ _ (R p) = evalState p initialGlossRenderState
    -- G.Translate (-170) (-20) -- shift to the middle of the window
    -- $ G.Scale 0.5 0.5          -- display it half the original size
    -- $ G.Text "Hello World"     -- text to display


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
    put initialGlossRenderState
    return $
      (G.Color fc $ G.Pictures $ map renderPolygon $ simplePolygons fr)
      `mappend` (G.Color lc $ G.Pictures $ map (renderTrail lw lcap lj) trails)
 where trails         = map calcTrail trs
       simplePolygons fr = tessRegion fr trails

renderTrail :: Double -> LineCap -> LineJoin -> [G.Point] -> G.Picture
renderTrail lw lcap lj pp =
  (G.Pictures $ map (renderLine lw) lines)
    `mappend` G.line pp
    `mappend` (renderCap cap $ swap $ head lines)
    `mappend` (renderCap cap $ last lines)
    `mappend` (G.Pictures $ map (renderJoin lj lwf) joins)
 where cap = case lcap of
               LineCapButt   -> mempty
               LineCapRound  -> G.circleSolid (lwf/2)
               LineCapSquare -> G.rectangleSolid lwf lwf
       lines = zip  pp (tail pp)
       joins = zip3 pp (tail pp) (tail $ tail pp)
       lwf    = realToFrac lw

renderCap :: G.Picture -> (G.Point, G.Point) -> G.Picture
renderCap cap ((x1, y1), (x2, y2)) =
  G.Translate x2 y2 $ G.Rotate (-angle) cap
 where vec   = (x2 - x1, y2 - y1)
       angle = G.radToDeg . G.argV $ vec

renderJoin :: LineJoin -> Float -> (G.Point, G.Point, G.Point) -> G.Picture
renderJoin lj lwf ((x1, y1), (x2, y2), (x3, y3)) =
  case lj of
    LineJoinMiter -> mempty
    LineJoinRound -> G.Translate x2 y2 $ G.circleSolid (lwf/2)
    LineJoinBevel -> G.Polygon [ (x2 + c1, y2 + c2)
                               , (x2 + c3, y2 + c4)
                               , (x2, y2)
                               ] `mappend`
                     G.Polygon [ (x2, y2)
                               , (x2 - c1, y2 - c2)
                               , (x2 - c3, y2 - c4)
                               ]
 where vec1   = (x2 - x1, y2 - y1)
       vec2   = (x3 - x2, y3 - y2)
       norm1   = G.mulSV (lwf/2) . G.normaliseV $ vec1
       norm2   = G.mulSV (lwf/2) . G.normaliseV $ vec2
       (c1, c2) = G.rotateV (tau/4) norm1
       (c3, c4) = G.rotateV (tau/4) norm2

renderLine :: Double -> (G.Point, G.Point) -> G.Picture
renderLine lw ((x1, y1), (x2, y2)) =
  G.Polygon [ (x1 + c1, y1 + c2)
            , (x1 - c1, y1 - c2)
            , (x2 - c1, y2 - c2)
            , (x2 + c1, y2 + c2)
            ]
 where lwf    = realToFrac lw
       vec    = (x2 - x1, y2 - y1)
       norm   = G.mulSV (lwf/2) . G.normaliseV $ vec
       (c1, c2) = G.rotateV (tau/4) norm

renderPolygon :: [G.Point] -> G.Picture
renderPolygon = G.Polygon


{- Points calculation-}

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
  (mconcat segments) `mappend` if closed then initLine else mempty
 where segments = scanl calcSeg initLine segs
       initLine = [(realToFrac x, realToFrac y)]

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
       step = 0.125


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
    Just a  -> modify (\s -> s{currentLineWidth = a})
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
changeDashing s = return ()

