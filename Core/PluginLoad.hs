--{-# LANGUAGE CPP #-}
module Core.PluginLoad 
( loadPlugin
, moduleNameToSourcePath
) where

import Control.Monad
import GHC
import GHC.Paths
import MonadUtils
import System.FilePath (pathSeparator)
import Data.Dynamic

loadPlugin :: String -> String -> IO (Maybe Dynamic)
loadPlugin symbol moduleName = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  liftIO $ putStrLn $ "Loading: " ++ moduleName ++ "." ++ symbol
  setSessionDynFlags dflags
  defaultCleanupHandler dflags $ do
    addTarget =<< guessTarget (moduleNameToSourcePath moduleName) Nothing
    r <- load LoadAllTargets
    case r of
      Failed -> return Nothing
      Succeeded -> liftM Just $
                     compile symbol moduleName

moduleNameToSourcePath :: String -> FilePath
moduleNameToSourcePath moduleName = 
  let translateCharacter '.'  = pathSeparator
      translateCharacter c = c
  in map translateCharacter moduleName ++ ".hs"

compile :: FilePath -> String -> Ghc Dynamic
compile symbol moduleName = do
{- #if MIN_VERSION_ghc(7,4,0)-}
  pr <- parseImportDecl "import Prelude"
  m <- parseImportDecl $ "import " ++ moduleName
  setContext [IIDecl m, IIDecl pr]
{- #else-}
  {-pr <- findModule (mkModuleName "Prelude") Nothing-}
  {-m <- findModule (mkModuleName moduleName) Nothing-}
{- #if MIN_VERSION_ghc(7,2,0)-}
  {-setContext [IIModule m, IIModule pr] []-}
{- #elif MIN_VERSION_ghc(7,0,0)-}
  {-setContext [] [(m, Nothing), (pr, Nothing)]-}
{- #else-}
  {-setContext [] [m, pr]-}
{- #endif-}
{- #endif-}
  dynCompileExpr (moduleName ++ "." ++ symbol)

