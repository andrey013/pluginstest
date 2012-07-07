module Plugin2.AnotherPlugin (plugin) where

import qualified Plugin1.Plugin

plugin :: (String -> String)
plugin = Plugin1.Plugin.plugin

