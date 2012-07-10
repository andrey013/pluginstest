module Plugins.Core.Main (plugin, main) where

import Plugins.Types

plugin = Plugin 
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  }

main = print "Core"
