module Main where

import PluginLoad
import Plugins.Types
import Control.Monad (forM_, when, filterM, foldM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import qualified Data.Map as Map


main :: IO ()
main = do
  names <- getDirectoryContents pluginsDir
  let properNames = (map (pluginsDir </>) . filter (`notElem` [".", ".."])) names
  dirNames <- filterM (doesDirectoryExist) properNames
  print dirNames
  plugins <- mapM loadPlugin' dirNames
  let exts = foldl func Map.empty plugins
  print (plugins :: [Maybe Plugin])
  print exts
 where
    func1 c d = Map.insertWith (++) (point d) (symbols d) c
    func a b = case b of 
                 Just bb -> foldl func1 a $ extentions bb
                 Nothing -> a
    pluginsDir = "Plugins"
    loadPlugin' name = do
      let moduleName = name </> "Main"
      loadPlugin "plugin" moduleName

