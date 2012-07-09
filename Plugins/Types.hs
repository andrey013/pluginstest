module Plugins.Types
  (Plugin(..)
  , Extension(..)
  ) where

data Plugin = Plugin
  { pluginId         :: String
  , extensionPoints  :: [String]
  , extentions       :: [Extension]
  , name             :: String
  , provider         :: String
  } deriving (Show)

data Extension = Extension
  { point            :: String
  , symbol           :: [String]
  } deriving (Show)

