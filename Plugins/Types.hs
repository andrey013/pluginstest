module Plugins.Types
  (Plugin(..)
  , Extension(..)
  ) where

data Plugin = Plugin
  { extentions       :: [Extension]
  , name             :: String
  } deriving (Show)

data Extension = Extension
  { point            :: String
  , symbols           :: [String]
  } deriving (Show)

