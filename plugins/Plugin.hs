module Plugin (plugin) where

plugin :: (String -> String)
plugin a = "Hello, " ++ a ++ "!"

