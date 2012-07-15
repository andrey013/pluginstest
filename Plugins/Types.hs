{-# LANGUAGE DeriveDataTypeable #-}

module Plugins.Types where

import Data.Dynamic
import Data.Maybe
import Graphics.Rendering.OpenGL

castMaybeDynamic :: Typeable t => Maybe Dynamic -> t
castMaybeDynamic a = case a of
  Just dp -> fromMaybe undefined (fromDynamic dp)
  Nothing -> undefined

data Plugin
  = Plugin
  { extentions       :: [Extension]
  , name             :: String
  } deriving (Show, Typeable)

data Extension
  = Extension
  { point            :: String
  , symbols          :: [String]
  } deriving (Show, Typeable)

data Core
  = Core
  {loadPlugin :: String -> IO [Maybe Dynamic]
  } deriving (Typeable)

data Window
  = Window
  { initWindow        :: IO ()
  , close             :: IO ()
  , frameDone         :: IO ()
  , windowCloseEvent  :: (IO ()) -> IO ()
  } deriving (Typeable)

data UpdateEvent
  = UpdateState Float
  | UpdateDisplay Float
  | CloseEvent

isStateEvent :: UpdateEvent -> Bool
isStateEvent (UpdateState _) = True
isStateEvent _ = False

data Application
  = Application
  { processState :: ApplicationState -> UpdateEvent -> ApplicationState
  } deriving (Typeable)

data ApplicationState
  = ApplicationState
  { angle :: Float
  , delta :: Float
  , position :: (Float, Float)
  , net :: MyndNode
  } deriving (Typeable)

data MyndNode
  = MyndNode
  { title :: String
  , tex :: Maybe TextureObject
  , width :: Int
  , children :: [MyndNode]
  } deriving (Typeable)

data Renderer
  = Renderer
  { render :: ApplicationState -> IO ()
  , resize :: (Int, Int) -> IO ()
  } deriving (Typeable)

