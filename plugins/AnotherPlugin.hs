module B (plugin) where

plugin :: (String -> String)
plugin a = "Bye, " ++ a ++ "!" ++ (show $ fibs !! 100)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)