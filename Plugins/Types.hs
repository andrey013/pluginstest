{-# LANGUAGE DeriveDataTypeable #-}
module Plugins.Types where

import Data.Dynamic
import Data.Maybe

data Plugin = Plugin
  { extentions       :: [Extension]
  , name             :: String
  } deriving (Show, Typeable)

data Extension = Extension
  { point            :: String
  , symbols          :: [String]
  } deriving (Show, Typeable)

data Core = Core
  {loadPlugin :: String -> IO [Maybe Dynamic]
  } deriving (Typeable)

castMaybeDynamic :: Typeable t => Maybe Dynamic -> t
castMaybeDynamic a = case a of
  Just dp -> fromMaybe undefined (fromDynamic dp)
  Nothing -> undefined
