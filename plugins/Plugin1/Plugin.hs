module Plugin1.Plugin (plugin) where

plugin :: (String -> String)
plugin a = "Hello, " ++ a ++ "!" ++ (show $ fibs !! 60)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
