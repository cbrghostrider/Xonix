-- -------------------------------------------------------------------------------------
--         Author: Sourabh S Joshi (cbrghostrider); Copyright - All rights reserved.
--                       For email, run on linux (perl v5.8.5):
--   perl -e 'print pack "H*","736f75726162682e732e6a6f73686940676d61696c2e636f6d0a"'
-- -------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import RenderingAPI
import XoData
import XoKeyboard
import XoPolyLine
import XoMotion
import XoRandom
import Data.IORef

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  texs <- initGLState progName
  initRandomizer
  game <- newIORef initGame
  setupCallbacks game texs
  mainLoop

setupCallbacks :: IORef XoGame -> [TextureObject] -> IO ()
setupCallbacks game texs = do
  displayCallback $= (display game texs)
  idleCallback $= Just (idle game)
  keyboardMouseCallback $= Just (keyboard game)
  reshapeCallback $= Nothing

display :: IORef XoGame -> [TextureObject] -> IO ()
display game texs = do 
  g <- get game
  clearColor $= convColor backgroundColor
  clear [ColorBuffer]
  displayCore g texs
  flush
  swapBuffers

idle :: IORef XoGame -> IO ()
idle game = do
  g <- get game
  newg <- idleFunc g
  game $= newg 
  postRedisplay Nothing

reshape :: IORef XoGame -> Size -> IO ()
reshape game s@(Size w h) = do
  postRedisplay Nothing
  return ()

keyboard :: IORef XoGame -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard game char state mod pos = do
  g <- get game
  game $= (processKey g char state mod pos)
  g' <- get game
  newg <- idleFunc g'
  game $= newg 
  postRedisplay Nothing

displayUnderlyingImage :: [TextureObject] -> IO ()
displayUnderlyingImage texs = do
  displayEntity $ XoEntity (XoRectangle (500, 500, -1) _ORIG_WIDTH _ORIG_HEIGHT) (1, 1, 1, 1) (Just(head texs)) (0, 0, 0)

displayCore :: XoGame -> [TextureObject] -> IO ()
displayCore g texs = preservingMatrix $ do
  loadIdentity
  displayUnderlyingImage texs
  mapM_ displayEntity $ boundingBoxes g
  if (points.shape.polyLine) g == []
    then return () 
    else let ptsToShow = (points $ shape $ polyLine g) ++ [(center $ shape $ player g)]
	     p = (polyLine g ){shape = ((shape $ polyLine g){points = ptsToShow})}
	 in  displayEntity p
  displayEntity $ player g
  mapM_ displayEntity $ balls g
  mapM_ displayEntity $ monsters g
  let (pupTime, pupType) = powerUp $ scoreBd g
  if (pupType) /= Nothing 
	then displayPowerUp (pupTime, pupType)
	else return ()
  displayScoreBoard $ scoreBd g
  displayCredits

 
  -- below is example of out of order bounding box vertices, which will NOT work with tesselation
  -- displayEntity $ XoEntity  (XoPolygon [(700, 700, 0), (800, 600, 0), (900, 800, 0), (800, 800, 0), (800, 900, 0), (650, 900, 0), (650, 600, 0), (675, 600, 0), (900, 600, 0), (675, 700, 0), (800, 700, 0)]) (0, 0, 1, 1) Nothing (0, 0, 0) 


