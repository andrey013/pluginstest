module Plugins.Types
  ( Plugin(..)
  , Extension(..)
  , Exts
  ) where

import qualified Data.Map as Map

data Plugin = Plugin
  { extentions       :: [Extension]
  , name             :: String
  } deriving (Show)

data Extension = Extension
  { point            :: String
  , symbols           :: [String]
  } deriving (Show)

type Exts = Map.Map String [(String, (String, Plugin))]

