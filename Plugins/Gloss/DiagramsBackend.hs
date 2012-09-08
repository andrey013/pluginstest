{-# Language FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
           , ViewPatterns
  #-}
module Plugins.Gloss.DiagramsBackend where

-- from base
import Data.Typeable
import Control.Monad.State
import System.IO.Unsafe

import qualified Graphics.Gloss as G
import qualified Graphics.Rendering.OpenGL as GL

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Path (getClip)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

import Data.List.Split      (chunksOf)

import Plugins.Types


data GlossBackend = GlossBackend
    deriving (Show, Typeable)

data GlossRenderState =
  GlossRenderState{ currentFillColor :: G.Color
                  , currentLineColor :: G.Color
                  , currentFillRule  :: GL.TessWinding
                  }

initialGlossRenderState = GlossRenderState
                            G.white
                            G.black
                            GL.TessWindingNonzero
                            -- GL.TessWindingOdd

type GlossRenderM = State GlossRenderState G.Picture

instance Monoid (Render GlossBackend v) where
  mempty      = R $ return mempty
  mappend (R a) (R b) =
    R $ do
      p1 <- a
      p2 <- b
      return $ p1 `mappend` p2

instance HasLinearMap v => Backend GlossBackend v where
  data Render GlossBackend v = R GlossRenderM
  type Result GlossBackend v = G.Picture
  data Options GlossBackend v = GlossOptions

  withStyle _ s _ (R p) = 
      R $ do
        case fc of
          Just (r, g, b, a) -> modify (\s -> s{currentFillColor = G.makeColor' (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)})
          Nothing           -> return ()
        case lc of
          Just (r, g, b, a) -> modify (\s -> s{currentLineColor = G.makeColor' (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)})
          Nothing           -> return ()
        p
   where fillColor = getFillColor <$> getAttr s
         fc = colorToRGBA <$> fillColor
         lineColor = getLineColor <$> getAttr s
         lc = colorToRGBA <$> lineColor

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

-- renderText :: Text -> G.Picture
renderText (Text tr _ str) = R . return $ G.text str

-- renderPath :: Path R2 -> G.Picture
renderPath (Path trs) =
  R $ do
    fc <- gets currentFillColor
    lc <- gets currentLineColor
    fr <- gets currentFillRule
    put initialGlossRenderState
    return $ (G.Color fc $ G.Pictures $ map renderPolygon $ simplePolygons fr)
      `mappend` (G.Color lc $ G.Pictures $ map renderTrail trails) -- G.Pictures $ map renderTrail trails)
 where trails         = map calcTrail trs
       -- complexPolygon = mconcat trails
       simplePolygons fr = mconcat $ map (tessRegion fr) trails-- complexPolygon
       -- simplePolygons fr = tessRegion fr $ mconcat trails-- complexPolygon

renderTrail :: [G.Point] -> G.Picture
renderTrail = G.line

renderPolygon :: [G.Point] -> G.Picture
renderPolygon = G.Polygon

tessRegion :: GL.TessWinding -> [G.Point] -> [[G.Point]]
tessRegion fr pp = renderSimplePolygon $ unsafePerformIO $
  GL.tessellate fr 0 (GL.Normal3 0 0 1)
  (\vv (GL.WeightedProperties (_,p) _ _ _) -> p) $
  GL.ComplexPolygon [GL.ComplexContour (map createVertex pp)] 
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
       step = 0.25
