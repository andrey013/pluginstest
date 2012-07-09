module Main where

import PluginLoad
import Plugins.Types
import Control.Monad (forM_, when)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))


main :: IO ()
main = do
  let pluginsDir = "Plugins"
  names <- getDirectoryContents pluginsDir
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = pluginsDir </> name
    isDirectory <- doesDirectoryExist path
    when isDirectory $ do 
      let moduleName = path </> "Main"
      a <- loadPlugin moduleName "plugin"
      case a of
        Just f  -> print (f :: Plugin)
        Nothing -> putStrLn "ERROR"

