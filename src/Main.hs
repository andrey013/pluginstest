module Main where

import PluginLoad
import Plugins.Types
import Control.Monad (forM_, when, filterM, foldM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import Control.Applicative ((<$>))
import qualified Data.Map as Map

pluginsDir = "Plugins"

main :: IO ()
main = do
  names <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents pluginsDir
  let properNames = map ((pluginsDir ++) . ('.':) . (++ ".Main")) names
  print properNames
  moduleNames <- filterM (doesFileExist . moduleNameToSourcePath) properNames
  print moduleNames
  plugins' <- mapM (loadPlugin "plugin") moduleNames
  let plugins = zip moduleNames plugins'
  print (plugins :: [(String, Maybe Plugin)])
  let exts = foldl func Map.empty plugins
  print exts
  core <- corePlugin exts
  case core of
    Just p  -> (p :: IO ())
    Nothing -> return ()
 where
    func1 p m e = Map.insertWith (++) (point e) (zip (symbols e) (repeat p)) m
    func m (pluginId,  Just p) = foldl (func1 (pluginId, p)) m $ extentions p
    func m (_, Nothing) = m
    corePlugin m = do
      let plugins = Map.lookup "Core" m
      case plugins of
        Just l  -> loadPlugin (fst . head $ l) (fst . snd . head $ l)
        Nothing -> return Nothing
