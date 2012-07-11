{-# LANGUAGE CPP #-}
module PluginLoad 
( loadPlugin
, moduleNameToSourcePath
) where

import Control.Monad
import GHC
import GHC.Paths
import Unsafe.Coerce
import System.FilePath (pathSeparator)

loadPlugin :: String -> String -> IO (Maybe a)
loadPlugin symbol moduleName = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags{importPaths = ["."]}
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

compile :: FilePath -> String -> Ghc a
compile symbol moduleName = do
#if MIN_VERSION_ghc(7,4,0)	
  pr <- parseImportDecl "import Prelude"
  m <- parseImportDecl $ "import " ++ moduleName
  setContext [IIDecl m, IIDecl pr]
#else
  pr <- findModule (mkModuleName "Prelude") Nothing
  m <- findModule (mkModuleName moduleName) Nothing
#if MIN_VERSION_ghc(7,2,0)
  setContext [IIModule m, IIModule pr] []
#elif MIN_VERSION_ghc(7,0,0)
  setContext [] [(m, Nothing), (pr, Nothing)]
#else
  setContext [] [m, pr]
#endif
#endif
  value <- compileExpr (moduleName ++ "." ++ symbol)
  return $ unsafeCoerce value

