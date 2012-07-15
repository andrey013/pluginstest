module Plugins.Core.Main (plugin, main) where

import Plugins.Types
import Data.Time
import Data.IORef
import Data.Maybe(listToMaybe)
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever,when,liftM )
import System.Exit ( exitSuccess )
import Reactive.Banana

import Graphics.Rendering.OpenGL
import Data.Vector.Storable( unsafeWith )
import Codec.Picture

plugin = Plugin 
  { extentions = [Extension "Core" ["main"]]
  , name = "1"
  }

main :: Core -> IO ()
main core = do

  tex <- makeTexture "res/pixelTest.png"

  (a:_) <- loadPlugin core "Window"
  let window = castMaybeDynamic a :: Window
  initWindow window

  (b:_) <- loadPlugin core "Application"
  let application = castMaybeDynamic b :: Application
  --initAppplication application

  (c:_) <- loadPlugin core "Renderer"
  let renderer = castMaybeDynamic c :: Renderer
  resize renderer (800, 600)
  --initAppplication application

  let --networkDescription :: forall t. NetworkDescription t ()
      networkDescription = do
        eClose <- event window
        ti <- timer 40 200
        let
          eStateUpdate = filterE isStateEvent ti
          eDispayUpdate = filterE (not . isStateEvent) ti
          --state :: Behavior t ApplicationState
          state = stepper (ApplicationState 0 0 (0,0)
                                   (MyndNode "" tex 0
                                    [ MyndNode "" tex 1
                                      [ MyndNode "" tex 4 [MyndNode "" Nothing 0 []]
                                      ]
                                    , MyndNode "" Nothing 2
                                       [ MyndNode "" Nothing 2 [MyndNode "" Nothing 0 []]
                                       , MyndNode "" Nothing 3 [MyndNode "" Nothing 0 []]
                                       , MyndNode "" Nothing 2 [MyndNode "" Nothing 0 []]
                                       ]
                                    , MyndNode "" Nothing 0 [MyndNode "" Nothing 0 []]
                                    ]
                                   )
                          ) $
                  ((processState application <$> state) <@> eStateUpdate)
        reactimate $ (close window >> exitSuccess) <$  eClose
        reactimate $ (\s -> render renderer s >> frameDone window) <$> state <@ eDispayUpdate
  network <- compile networkDescription
  actuate network
  threadDelay 5000000

event :: Window -> NetworkDescription t (Event t UpdateEvent)
event window = do
      (addHandler, fire) <- liftIO newAddHandler
      liftIO $ windowCloseEvent window $ fire CloseEvent
      fromAddHandler addHandler

timer :: Int -> Int -> NetworkDescription t (Event t UpdateEvent)
timer d s = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ forkIO $ do
    t <- getCurrentTime
    r <- newIORef t
    forever $ do
      threadDelay (d * 10 ^ 3)
      oldTime <- readIORef r
      newTime <- getCurrentTime
      let diff = fromEnum $ diffUTCTime newTime oldTime / 10 ^ 9
      if diff > s
        then do
          let newOldTime = addUTCTime (fromIntegral s / 10 ^ 3) oldTime
          writeIORef r newOldTime
          fire $ UpdateState $ fromIntegral s
          fire $ UpdateDisplay $ (fromIntegral . fromEnum) (diffUTCTime newTime newOldTime) / (fromIntegral s * 10 ^ 9)
        else
          fire $ UpdateDisplay $ (fromIntegral . fromEnum) (diffUTCTime newTime oldTime) / (fromIntegral s * 10 ^ 9)
  fromAddHandler addHandler

makeTexture :: String -> IO (Maybe TextureObject)
makeTexture filename = do
  (Right qwe) <- readImage filename
  (Right (ImageRGBA8 (Image width height pixels))) <- readImage filename

  exts <- get glExtensions
  texture <- if "GL_EXT_texture_object" `elem` exts
                 then liftM listToMaybe $ genObjectNames 1
                 else return Nothing
  textureBinding Texture2D $= texture

  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  unsafeWith pixels $ texImage2D Nothing NoProxy 0 RGBA'
                          (TextureSize2D (fromIntegral width) (fromIntegral height))
                          0 . PixelData RGBA UnsignedByte
  return texture

