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

import qualified Graphics.Gloss as G

-- from diagrams-lib
import Diagrams.Prelude
import Diagrams.TwoD.Path (getClip)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text

import Plugins.Types


data GlossBackend = GlossBackend
    deriving (Show, Typeable)

instance Monoid (Render GlossBackend v) where
  mempty      = GlossBackendRender mempty
  mappend (GlossBackendRender a) (GlossBackendRender b) = GlossBackendRender $ a `mappend` b

instance HasLinearMap v => Backend GlossBackend v where
  data Render GlossBackend v = GlossBackendRender G.Picture
  type Result GlossBackend v = G.Picture
  data Options GlossBackend v = GlossOptions

  withStyle _ _ _ a = a
  doRender _ _ (GlossBackendRender a)
    = G.Scale 30 30 a
    -- G.Translate (-170) (-20) -- shift to the middle of the window
    -- $ G.Scale 0.5 0.5          -- display it half the original size
    -- $ G.Text "Hello World"     -- text to display


instance Renderable (Segment R2) GlossBackend where
  render c = render c . flip Trail False . (:[])

instance Renderable (Trail R2) GlossBackend where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Path R2) GlossBackend where
  render _ = GlossBackendRender . renderPath

instance Renderable Text GlossBackend where
  render _ = GlossBackendRender . renderText

renderText :: Text -> G.Picture
renderText _ = mempty

renderPath :: Path R2 -> G.Picture
renderPath (Path trs)  = mconcat $ map renderTrail trs

renderTrail :: (P2, Trail R2) -> G.Picture
renderTrail (unp2 -> (x,y), Trail segs closed) =
  -- m x y
  (mconcat $ map fst segments) `mappend` if closed then G.line [(realToFrac x, realToFrac y), (x0, y0)] else mempty
 where (_, (x0,y0)) = last segments
       initLine = (G.blank, (realToFrac x, realToFrac y))
       segments = scanl renderSeg initLine segs

renderSeg :: (G.Picture, G.Point) -> Segment R2 -> (G.Picture, G.Point)
renderSeg (_, (x,y)) (Linear (unr2 -> (x0,y0))) =
  (G.line
    [(x,y)
    ,(x + realToFrac x0, y + realToFrac y0)]
  ,(x + realToFrac x0, y + realToFrac y0))
renderSeg (_, (x,y)) (Cubic  (unr2 -> (x0,y0)) (unr2 -> (x1,y1)) (unr2 -> (x2,y2))) =
  (G.line
    [(x,y)
    ,(x + realToFrac x0, y + realToFrac y0)
    ,(x + realToFrac x1, y + realToFrac y1)
    ,(x + realToFrac x2, y + realToFrac y2)]
  ,(x + realToFrac x2, y + realToFrac y2))

