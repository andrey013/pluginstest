module Core.Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when, filterM, foldM, liftM, liftM2)
import Data.Dynamic
import Plugins.Types
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import qualified Data.Map as Map
import qualified Core.PluginLoad as PL

pluginsDir = "Plugins"

type Exts = Map.Map String [(String, (String, Plugin))]

loadPlugin' :: Exts -> [String] -> [String] -> String -> String -> IO [Maybe Dynamic]
loadPlugin' m imp pack t s = do
  let plugins = Map.lookup s m
  case plugins of
    Just l  -> mapM (\p -> PL.loadPlugin pack imp (fst . snd $ p) t (fst p)) l
    Nothing -> return []

realMain :: IO ()
realMain = do
  names <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents pluginsDir
  let properNames = map ((pluginsDir ++) . ('.':) . (++ ".Main")) names
  -- print properNames
  moduleNames <- filterM (\name -> liftM2 (||) (doesFileExist $ (PL.moduleNameToSourcePath name) ++ ".lhs") (doesFileExist $ (PL.moduleNameToSourcePath name) ++ ".hs")) properNames
  -- print moduleNames
  plugins' <- mapM (\name -> PL.loadPlugin [] ["Plugins.Types"] name "Plugin" "plugin") moduleNames
  let plugins = zip moduleNames plugins'
  -- print (plugins :: [(String, Maybe Dynamic)])
  let exts = foldl func0 Map.empty plugins
  print exts
  (core:_) <- loadPlugin' exts [] ["Plugins.Types"] "Core -> IO ()" "Core"
  (castMaybeDynamic core :: Core -> IO ()) Core {loadPlugin = loadPlugin' exts, evaluateString = PL.loadPlugin }
 where
    func2 p m e = Map.insertWith (++) (point e) (zip (symbols e) (repeat p)) m
    func1 m (pluginId,  Just p) = foldl (func2 (pluginId, p)) m $ extentions p
    func1 m (_, Nothing) = m
    func0 m (pluginId,  Just dp) = func1 m (pluginId, fromDynamic dp)
    func0 m (_, Nothing) = m
