{-# LANGUAGE CPP #-}

module Main where

import Control.Monad
import GHC
import GHC.Paths
import Unsafe.Coerce

loadPlugin :: FilePath -> IO (String -> String)
loadPlugin pluginName = do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    defaultCleanupHandler dflags $ do
      addTarget =<< guessTarget "plugins/Plugin.hs" Nothing
      r <- load LoadAllTargets
      case r of
        Failed -> error $ "Error loading plugin"
        Succeeded -> return ()

#if MIN_VERSION_ghc(7,4,0)	
      pr <- parseImportDecl "import Prelude"
      m <- parseImportDecl "import Plugin"
      setContext [IIDecl m, IIDecl pr]
#else
      pr <- findModule (mkModuleName "Prelude") Nothing
      m <- findModule (mkModuleName "Plugin") Nothing
#if MIN_VERSION_ghc(7,2,0)
      setContext [IIModule m, IIModule pr] []
#elif MIN_VERSION_ghc(7,0,0)
      setContext [] [(m, Nothing), (pr, Nothing)]
#else
      setContext [] [m, pr]
#endif
#endif
	  
	  
      value <- compileExpr ("Plugin.plugin :: (String -> String)")
      let value' = (unsafeCoerce value) :: (String -> String)
      return value'

main :: IO ()
main = do
  a <- loadPlugin "plugins/Plugin.hs"
  putStrLn $ a "Andrey"

