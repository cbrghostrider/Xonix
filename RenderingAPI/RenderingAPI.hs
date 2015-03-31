----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
--	ssjoshi - Rendering API used to render shapes using HOpenGL
----------------------------------------------------------------------------------------
--	Based primarily on Sven Panitz's tutorial on HOpenGL (excellent tutorial BTW!)
--	www.cs.hs-rm.de/~panitz/hopengl/skript.pdf 
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

module RenderingAPI 
  ( convColor, convCoord, 
    initGLState, displayEntity, displayScoreBoard, displayCredits, displayPowerUp, 
    loadTexture ) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.Either 
import Data.List 
import GHC.Float

import Control.Monad(when)
import System.Exit(exitFailure)
import System.IO(withBinaryFile, IOMode(ReadMode), openBinaryFile, hGetBuf)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Ptr(Ptr())
import Foreign.Marshal.Alloc(mallocBytes)


import XoData

type APIPoint = (GLfloat, GLfloat, GLfloat)

convColor :: XoColor -> Color4 GLfloat
convColor (r, g, b, op) = Color4 r g b op 

convCoord :: XoPoint3D -> APIPoint
convCoord (x, y, _) = (xc, yc, 0)
  where
    xc = fromIntegral (x - (_PLAY_WINDOW_WIDTH `div` 2)) / fromIntegral (_PLAY_WINDOW_WIDTH `div` 2)
    yc = fromIntegral (y - (_PLAY_WINDOW_HEIGHT `div` 2)) / fromIntegral (_PLAY_WINDOW_HEIGHT `div` 2)
----------------------------------------------------------------------------------------
--				MAIN RENDERING FUNCTIONS
----------------------------------------------------------------------------------------
initGLState :: String -> IO ([TextureObject])
initGLState prog = do
  clearColor 		$= convColor backgroundColor
  blend 		$= Enabled
  blendFunc 		$= (SrcAlpha, OneMinusSrcAlpha)
  lineSmooth 		$= Enabled
  pointSmooth		$= Enabled
  polygonSmooth		$= Enabled
  initialDisplayMode 	$= [DoubleBuffered]
  initialWindowSize 	$= Size _GAME_WINDOW_WIDTH _GAME_WINDOW_HEIGHT
  lighting		$= Enabled
  ambient (Light 0)	$= Color4 0.2 0.2 0.2 1
  diffuse (Light 0)	$= Color4 0.9 0.9 0.9 1
  light (Light 0)	$= Enabled
  createWindow prog
  texture Texture2D $= Enabled 
  tex <- loadTexture ".\\Images\\intel.bmp"
  texture Texture2D $= Disabled 
  return [tex]
  
displayScoreBoard :: XoScoreBoard -> IO ()
displayScoreBoard scorebd = preservingMatrix $ do
  translate $ Vector3 xc yc (0::GLfloat)
  scale 0.0005 0.0005 (0.0005::GLfloat)
  currentColor $= (Color4 0 1 0 1)
  renderString Roman "Level:"
  currentColor $= (Color4 1 0 0 1)
  renderString Roman $ show (level scorebd)
  currentColor $= (Color4 0 1 0 1)
  renderString Roman "   Lives:"
  currentColor $= (Color4 1 0 0 1)
  renderString Roman $ show (lives scorebd)
  currentColor $= (Color4 0 1 0 1)
  renderString Roman "   Score:"
  currentColor $= (Color4 1 0 0 1)
  renderString Roman $ show (score scorebd)
  currentColor $= (Color4 0 1 0 1)
  renderString Roman "   Area:"
  currentColor $= (Color4 1 0 0 1)
  renderString Roman $ show (areapc scorebd)
  currentColor $= (Color4 0 1 0 1)
  renderString Roman "%   Power:"
  currentColor $= (Color4 1 0 0 1)
  let (pupTime, pup) = powerUp scorebd
  if (pup == Nothing) 
	then renderString Roman "Nothing"
	else let Just(center@(cx, cy, cz), pupTyp) = pup
	     in  case pupTyp of
		   XoLife	-> if (cx == 0 && cy == 0 && cz == 0) then do renderString Roman "Life" else do return ()
		   XoGrenade _	-> if (cx == 0 && cy == 0 && cz == 0) then do renderString Roman "Grenade" else do return ()
		   _		-> do renderString Roman "Unknown"
			
    where
      (xc, yc, zc) = convCoord (5, 5, 0)

displayCredits :: IO ()
displayCredits = preservingMatrix $ do
  translate $ Vector3 xc yc (0::GLfloat)
  scale 0.0003 0.0003 (0.0003::GLfloat)
  currentColor $= (Color4 0 0 1 1)
  renderString Roman "Haskell Xonix (pre-alpha) - ssjoshi"
    where
      (xc, yc, zc) = convCoord (5, 980, 0)
 
displayPowerUp :: (Int, Maybe (XoPoint3D, XoPowerUp)) -> IO ()
displayPowerUp power= preservingMatrix $ do
  translate $ Vector3 xc yc (0::GLfloat)
  rotate (fromIntegral turnAngle) $ Vector3 0 0 (1::GLfloat)
  renderStar (fromIntegral starSizeOuter / fromIntegral _GAME_WINDOW_WIDTH) 7 
		(concat $ repeat $ [Color4 1 0 0 1, Color4 0 1 0 1, Color4 0 0 1 1])
  currentColor $= innerColor
  renderStar (fromIntegral starSizeInner / fromIntegral _GAME_WINDOW_WIDTH) 5 (repeat innerColor)
    where
      (frameRem, Just (place, pup)) = power
      turnAngle = frameRem `mod` 360
      (xc, yc, zc) = convCoord place
      innerColor = case pup of
	XoLife 		-> Color4 1 0 0 1
	XoGrenade _	-> Color4 0 0 1 1
	XoSlowTime _ 	-> Color4 0 1 0 1
	XoTurbo _ 	-> Color4 1 1 0 1
	_		-> Color4 0 0 0 1

displayEntity :: XoEntity -> IO ()
displayEntity ent = preservingMatrix $ do
  currentColor $= (convColor $ coloring ent)
  displayShape (texOfEntity ent) (shape ent)
  
displayShape :: Maybe TextureObject -> XoShape -> IO ()
displayShape tex (XoRectangle cen wd ht) = preservingMatrix $ do
  let wdh = wd `div` 2
  let hth = ht `div` 2
  let (xc, yc, zc) = cen
  let points = [(xc-wdh, yc-hth, 0), (xc+wdh, yc-hth, 0), (xc+wdh, yc+hth, 0), (xc-wdh, yc+hth, 0)]
  if (tex == Nothing) 
    then do renderData Quads $ map convCoord points
    else do renderRectangleWithTexture tex Quads ([(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)], map convCoord points)
 
displayShape tex (XoCircle cen rad) = preservingMatrix $ do 
  displayAt cen $ fillCircle (fromIntegral rad / fromIntegral _NORMALIZATION_FACTOR)

displayShape tex (XoDiamond cen wd) = preservingMatrix $ do
  let wdh = truncate (sqrt (2.0 * fromIntegral wd * fromIntegral wd)) `div` 2
  let (xc, yc, zc) = cen
  let points = [(xc, yc-wdh, 0), (xc+wdh, yc, 0), (xc, yc+wdh, 0), (xc-wdh, yc, 0)]
  renderData Quads $ map convCoord points

displayShape tex (XoLine p1 p2) = preservingMatrix $ do
  origLineWidth <- get lineWidth
  lineWidth $= (fromIntegral polyLineWidth)
  renderPrimitive Lines $ makeVertices $ map convCoord [p1, p2]
  lineWidth $= origLineWidth 

displayShape tex (XoTraceLine ps) = preservingMatrix $ do
  origLineWidth <- get lineWidth
  lineWidth $= (fromIntegral polyLineWidth)
  renderPrimitive LineStrip $ makeVertices $ map convCoord ps
  lineWidth $= origLineWidth 

displayShape tex (XoPolygon ps) = preservingMatrix $ do
  startess <- tessellate TessWindingPositive 0 (Normal3 0 0 0) noOpCombiner $ complexPolygon $ map (\(x, y, z) -> Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)) $ map convCoord $ ps
  drawSimplePolygon startess

-- main display API, which ties user defined render function to OpenGL callback
-- this also initializes OpenGL, and creates the window for drawing
renderInWindow :: IO () -> IO () 
renderInWindow renderFunc = do
  (progName, _) <- getArgsAndInitialize
  createWindow progName
  displayCallback $= renderFunc
  mainLoop

--render as oglPrim the given data of (x, y, z) points
renderData :: PrimitiveMode -> [APIPoint] -> IO ()
renderData oglPrimShape ps = do
  renderAs oglPrimShape ps
  flush

renderRectangleWithTexture :: Maybe TextureObject -> PrimitiveMode -> ([APIPoint], [APIPoint]) -> IO ()
renderRectangleWithTexture tex oglPrimShape (ts, ps) = do
  texture Texture2D $= Enabled     
  textureBinding Texture2D $= tex
  blend $= Disabled
  let (t1:t2:t3:t4:ts') = ts
  let (p1:p2:p3:p4:ps') = ps
  let t (x, y, _) = texCoord $ TexCoord2 x y  
  let v (x, y, z) = vertex $ Vertex3 x y z
  renderPrimitive oglPrimShape $ do
    t t1 >> v p1
    t t2 >> v p2
    t t3 >> v p3
    t t4 >> v p4
  flush
  textureBinding Texture2D $= Nothing
  texture Texture2D $= Disabled
  blend $= Enabled

-- a render helper, which helps create the appropriate render function for us
renderAs :: PrimitiveMode -> [APIPoint] -> IO ()
renderAs oglPrim ps = renderPrimitive oglPrim $ makeVertices ps

--a function to convert vertices into an IO () for rendering
--CAUTION: vertex if not called within renderPrimitive, 
--         results in unspecified operation (from OGL standard) 
makeVertices :: [APIPoint] -> IO ()
makeVertices = mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)

displayAt :: XoPoint3D -> IO () -> IO () 
displayAt coord displayMe = do
  translate $ Vector3 xc yc (0::GLfloat)
  displayMe
  loadIdentity
    where
      (xc, yc, zc) = convCoord coord 

----------------------------------------------------------------------------------------
--		    RENDERING FOR RECTANGLES, SQUARES, ETC
----------------------------------------------------------------------------------------
renderRect :: GLfloat ->GLfloat -> IO ()
renderRect width height = 
  renderData Quads [(w, h, 0), (w, (-1) * h, 0), ((-1) * w, (-1) * h, 0), ((-1) * w, h, 0)]
    where
      w = width/2
      h = height/2

renderSquare :: GLfloat -> IO ()
renderSquare width = renderRect width width

renderRotatedSquare :: GLfloat -> GLfloat -> IO ()
renderRotatedSquare alpha width = do
  rotate alpha $ Vector3 0 0 (1::GLfloat)
  renderSquare width

----------------------------------------------------------------------------------------
--		    RENDERING FOR CIRCLE, RING, AND CURVES
----------------------------------------------------------------------------------------

-- converting a circle into a 2D list of points on circle in order (size of list numPts)
circlePoints :: (Integral b) => GLfloat -> b -> [APIPoint]
circlePoints rad numPts 
  = [let alpha = twoPi * fromIntegral i / fromIntegral numPts 
     in ((rad * (sin alpha)), (rad * (cos alpha)), 0::GLfloat) | i <- [1, 2..numPts]]
    where
      twoPi = 2*pi

-- create a circle of radius r (approx with 100 points)
circle :: GLfloat -> [APIPoint]
circle r = circlePoints r 100

--get all Points for rendering a ring
ringPoints :: GLfloat -> GLfloat -> [APIPoint]
ringPoints rInner rOuter = concat $ map (\(x, y) -> [x, y]) (points ++ [p])
  where
    points@(p:_) = zip (circle rInner) (circle rOuter)

--render a circle with radius r with n points
renderCircleApprox :: (Integral b) => GLfloat -> b -> IO ()
renderCircleApprox r n = renderData LineLoop $ circlePoints r n

--render a circle of radius r
renderCircle :: GLfloat -> IO ()
renderCircle r = renderData LineLoop $ circle r

--render a circle of radius r, filled 
fillCircle :: GLfloat -> IO ()
fillCircle r = renderData Polygon $ circle r

--render a ring
renderRing :: GLfloat -> GLfloat -> IO ()
renderRing ri ro = renderData QuadStrip (ringPoints ri ro)

renderRingAt :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderRingAt x y ri ro = do
  translate $ Vector3 x y (0::GLfloat)
  renderRing ri ro

----------------------------------------------------------------------------------------
--			RENDERING STARS
----------------------------------------------------------------------------------------

starPoints :: Integral b => GLfloat -> b -> [APIPoint] 
starPoints radius rays = map (\(_, (x, y, z)) -> (x, y, z)) (os ++ es)
  where
    (os, es) = partition (\(i,_) -> odd i) $ zip [1, 2..] $ circlePoints radius rays

complexPolygon :: Num a => [Vertex3 GLdouble] -> ComplexPolygon a
complexPolygon points = ComplexPolygon [ComplexContour $ map (\v -> AnnotatedVertex v 0) points]

complexPolygonAnn :: [AnnotatedVertex (Color4 GLfloat)] -> ComplexPolygon (Color4 GLfloat)
complexPolygonAnn points = ComplexPolygon [ComplexContour points]

renderStar :: (Integral b) => GLfloat -> b -> [Color4 GLfloat] -> IO ()
renderStar radius rays colors = do
  startess 
    <- tessellate 
         TessWindingPositive 0 (Normal3 0 0 0) colorCombiner 
            $ complexPolygonAnn (map (\((x, y, z), color) -> 
		AnnotatedVertex (Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)) color) 
		  (zip (starPoints radius rays) colors) )
  let (SimplePolygon primParts) = startess
  mapM_ renderAnnotatedPrimParts primParts

renderAnnotatedPrimParts (Primitive mode annVerts) =
  renderPrimitive mode $ mapM_ (\(AnnotatedVertex vert col) -> do
				    currentColor $= col
				    vertex vert) annVerts

drawSimplePolygon :: SimplePolygon a -> IO ()
drawSimplePolygon (SimplePolygon primitiveParts) = mapM_ renderPrimitiveParts primitiveParts

renderPrimitiveParts :: Primitive a -> IO()
renderPrimitiveParts (Primitive primitiveMode vertices) = 
  renderPrimitive primitiveMode $ mapM_ (vertex . stripAnnotation) vertices

stripAnnotation :: AnnotatedVertex a -> Vertex3 GLdouble
stripAnnotation (AnnotatedVertex plainVertex _) = plainVertex

noOpCombiner :: a -> b -> GLfloat
noOpCombiner _newVertex _weightedproperties = 0.0 ::GLfloat

colorCombiner :: Combiner (Color4 GLfloat)
colorCombiner 
  _newVertex
   (WeightedProperties
      (w0, Color4 r0 g0 b0 a0)
      (w1, Color4 r1 g1 b1 a1)
      (w2, Color4 r2 g2 b2 a2)
      (w3, Color4 r3 g3 b3 a3)) =
         Color4 (w0*r0 + w1*r1 + w2*r2 + w3*r3)
                (w0*g0 + w1*g1 + w2*g2 + w3*g3)
                (w0*b0 + w1*b1 + w2*b2 + w3*b3)
  		1

----------------------------------------------------------------------------------------
--			TEXTURE MAPPING
----------------------------------------------------------------------------------------

-- texture mapping load/upload routine based on code from Claude Heiland-Allen
-- http://haskell.1045720.n5.nabble.com/Texture-Mapping-td3197156.html

loadTexture :: FilePath -> IO TextureObject
loadTexture f = do
   withBinaryFile f ReadMode $ \h -> do
     let bytes = 1024 * 1024 * 3
     allocaBytes bytes $ \pixels -> do
       bytes' <- hGetBuf h pixels bytes
       when (bytes' /= bytes) exitFailure
       texture Texture2D $= Enabled
       [tex] <- genObjectNames 1
       textureBinding Texture2D $= Just tex
       build2DMipmaps Texture2D RGB' 1024 1024 (PixelData RGB UnsignedByte pixels)
       textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
       textureWrapMode Texture2D S $= (Repeated, Repeat)
       textureWrapMode Texture2D T $= (Repeated, Repeat)
       textureBinding Texture2D $= Nothing
       texture Texture2D $= Disabled
       return tex




