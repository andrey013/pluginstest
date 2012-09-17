{-# LANGUAGE NoMonomorphismRestriction
           , DeriveDataTypeable
           , StandaloneDeriving
  #-}
module Plugins.Gallery.Main (plugin, application) where

import System.IO
import Control.Monad (forM_, when, filterM, foldM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import Data.List
import Data.List.Split
import Plugins.Types
import Plugins.Gloss.DiagramsBackend
import Diagrams.Prelude
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Text.Pandoc 

plugin :: Plugin
plugin = Plugin 
  { extentions = [Extension "Application" ["application"]]
  , name = "1"
  }

application ::  Application GlossBackend
application = Application
  { processState = processState'
  , processKey   = processKey'
  , initApp      = initApp'
  }

processState' :: Float -> ApplicationState GlossBackend  -> ApplicationState GlossBackend
processState' a st | angle st == 0 =
  st{ --diagram = (-- F.example ||| Gr.example ||| Hi.example 
                -- (eqTriangle 3 ||| exampleL)
                -- === (exampleM  ||| exampleN)
                -- M.example ||| Pa.example ||| Pe.example
                -- Q.example ||| Si.example ||| St.example
                -- T.example
               -- eqTriangle 3
             -- :: Diagram GlossBackend R2)
              -- , 
               angle = 300}
                   | otherwise     = st{angle = angle st + (0.01 * delta st * realToFrac a)}

processKey' :: Event -> ApplicationState a -> ApplicationState a
processKey' (G.EventKey (G.Char 'j') G.Up _ _)  s = s{ n = (n s) + 1}
processKey' (G.EventKey (G.Char 'k') G.Up _ _)  s = s{ n = (n s) - 1}
processKey' _  s = s

galleryDir = "Plugins/Gallery/Gallery"
galleryModule = "Plugins.Gallery.Gallery"

initApp' :: Core -> ApplicationState GlossBackend  -> IO (ApplicationState GlossBackend )
initApp' core st = do
  names <- liftM (filter (\e -> any (`isSuffixOf` e) ["27.hs" , ".~lhs"])) $ getDirectoryContents galleryDir
  let properNames = map ((galleryModule ++) . ('.':)) $ map (head . splitOn ".") names
  print $ properNames

  diagrams <- mapM (\name -> evaluateString core
    [ "Diagrams.Prelude"
    , "Data.Typeable"
    , "Data.Monoid"
    ]
    ["Plugins.Gloss.DiagramsBackend"]
    name
    "Diagram GlossBackend R2" "example") $ sort properNames
  --let (first:_) = diagrams
  --    d = (castMaybeDynamic first :: Diagram GlossBackend R2)
  return st{ diagrams = (map castMaybeDynamic diagrams) :: [Diagram GlossBackend R2]}

initApp'' :: Core -> ApplicationState GlossBackend  -> IO (ApplicationState GlossBackend )
initApp'' core st = do
  file <- readFile "Plugins/Gallery/Gallery/diagrams-manual.rst" 
  let Pandoc _ manual = readRST defaultParserState file
      codeSamples     = filter chooseDiagrams manual
  mapM_ writeManualFile $ zip [1..] codeSamples
  return st
 where chooseDiagrams (CodeBlock _ _) = True
       chooseDiagrams _               = False
       writeManualFile (n, CodeBlock _ str) =
         writeFile
           ("Plugins/Gallery/Gallery/Manual" ++ show n ++ ".hs") $
           unlines
           [ "{-# LANGUAGE NoMonomorphismRestriction"
           , "           #-}"
           , "module Plugins.Gallery.Gallery.Manual" ++ show n ++ " where"
           , "import Diagrams.Prelude"
           , ""
           , str
           ]


-- fibs ::  [Integer]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
