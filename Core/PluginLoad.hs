--{-# LANGUAGE CPP #-}
module Core.PluginLoad 
( loadPlugin
, moduleNameToSourcePath
) where

import Control.Monad
import GHC
import DynFlags
import GHC.Paths
import MonadUtils
import System.FilePath (pathSeparator)
import Data.Dynamic

loadPlugin :: [String] -> [String] -> String -> String -> String -> IO (Maybe Dynamic)
loadPlugin packages modules moduleName t symbol =
  runGhc (Just libdir) $ do
    dflags' <- getSessionDynFlags
    let dflags = dflags'{ optLevel = 2
                        , extraPkgConfs = ["./cabal-dev/packages-7.4.1.conf"]
                        -- , buildTag = "p"
                        }
    liftIO $ putStrLn $ "Loading: " ++ moduleName ++ "." ++ symbol
    setSessionDynFlags dflags
    defaultCleanupHandler dflags $ do
      addTarget =<< guessTarget ({-moduleNameToSourcePath-} moduleName) Nothing
      mapM_ addTarget [Target (TargetModule $ mkModuleName x)
                       True Nothing | x <- modules]
      r <- load LoadAllTargets
      case r of
        Failed -> return Nothing
        Succeeded -> liftM Just $
                       compile packages modules moduleName t symbol

moduleNameToSourcePath :: String -> FilePath
moduleNameToSourcePath moduleName = 
  let translateCharacter '.'  = pathSeparator
      translateCharacter  c   = c
  in map translateCharacter moduleName -- ++ ".hs"

compile :: [String] -> [String] -> String -> String -> String -> Ghc Dynamic
compile packages modules moduleName t symbol = do
{- #if MIN_VERSION_ghc(7,4,0)-}
  liftIO $ putStrLn $ "Compiling: " ++ moduleName ++ "." ++ symbol
  i <- mapM (parseImportDecl . ("import " ++)) $
    ["Prelude"
    , moduleName
    ] ++ packages
      ++ modules
  {- pr <- parseImportDecl "import Prelude"
  m <- parseImportDecl $ "import " ++ moduleName
  ty <- parseImportDecl $ "import Plugins.Types"
  gl <- parseImportDecl $ "import Plugins.Gloss.DiagramsBackend"
  -}
  setContext $ map IIDecl i
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
  dynCompileExpr (moduleName ++ "." ++ symbol ++ " :: " ++ t)

