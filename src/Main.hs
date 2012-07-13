module Main where

import qualified PluginLoad as PL
import Plugins.Types
import Control.Monad (forM_, when, filterM, foldM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import Control.Applicative ((<$>))
import Data.Dynamic
import qualified Data.Map as Map

pluginsDir = "Plugins"

type Exts = Map.Map String [(String, (String, Plugin))]

loadPlugin' :: Exts -> String -> IO [Maybe Dynamic]
loadPlugin' m s = do
  let plugins = Map.lookup s m
  case plugins of
    Just l  -> mapM (\p -> PL.loadPlugin (fst p) (fst . snd $ p)) l
    Nothing -> return []

main :: IO ()
main = do
  names <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents pluginsDir
  let properNames = map ((pluginsDir ++) . ('.':) . (++ ".Main")) names
  -- print properNames
  moduleNames <- filterM (doesFileExist . PL.moduleNameToSourcePath) properNames
  -- print moduleNames
  plugins' <- mapM (PL.loadPlugin "plugin") moduleNames
  let plugins = zip moduleNames plugins'
  -- print (plugins :: [(String, Maybe Dynamic)])
  let exts = foldl func0 Map.empty plugins
  -- print exts
  (core:_) <- loadPlugin' exts "Core"
  (castMaybeDynamic core :: Core -> IO ()) Core {loadPlugin = loadPlugin' exts}
 where
    func2 p m e = Map.insertWith (++) (point e) (zip (symbols e) (repeat p)) m
    func1 m (pluginId,  Just p) = foldl (func2 (pluginId, p)) m $ extentions p
    func1 m (_, Nothing) = m
    func0 m (pluginId,  Just dp) = func1 m (pluginId, fromDynamic dp)
    func0 m (_, Nothing) = m
