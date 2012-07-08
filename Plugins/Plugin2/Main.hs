module Plugins.Plugin2.Main (plugin) where

import qualified Plugins.Plugin1.Main

plugin :: (String -> String)
plugin _ = Plugins.Plugin1.Main.plugin "a"

