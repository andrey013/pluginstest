{-# LANGUAGE CPP #-}
module PluginLoad (loadPlugin) where

import Control.Monad
import GHC
import GHC.Paths
import Unsafe.Coerce

loadPlugin :: FilePath -> String -> IO (Maybe a)
loadPlugin sourcePath symbol = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags
  defaultCleanupHandler dflags $ do
    addTarget =<< guessTarget (sourcePath ++ ".hs") Nothing
    r <- load LoadAllTargets
    case r of
      Failed -> return Nothing
      Succeeded -> liftM Just $
                     compile (sourcePathToModuleName sourcePath) symbol

sourcePathToModuleName :: FilePath -> FilePath
sourcePathToModuleName moduleName = 
  let translateCharacter '/'  = '.'
      translateCharacter '\\' = '.'
      translateCharacter c = c
  in map translateCharacter moduleName

compile :: FilePath -> String -> Ghc a
compile moduleName symbol = do
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

