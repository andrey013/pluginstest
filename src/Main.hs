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
      pr <- parseImportDecl "import Prelude"
      m <- parseImportDecl "import Plugin"
      setContext [IIDecl m, IIDecl pr]

      value <- compileExpr ("Plugin.plugin :: (String -> String)")
      let value' = (unsafeCoerce value) :: (String -> String)
      return value'

main :: IO ()
main = do
  a <- loadPlugin "plugins/Plugin.hs"
  putStrLn $ a "Andrey"
