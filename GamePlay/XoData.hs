--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoData where 

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.List

----------------------------------------------------------------------------------------
--			data types defined for game data structures
----------------------------------------------------------------------------------------
-- the time component of gameplay
type XoTime = Float

-- the 3D point any entity centers at, or consists of
type XoPoint3D = (GLsizei, GLsizei, GLsizei)
type XoPoints = [XoPoint3D]

-- the color any entity refers to 
type XoColor = (GLfloat, GLfloat, GLfloat, GLfloat)

-- the direction any entity is moving. This is the vector made w/ origin
type XoDirection = (GLsizei, GLsizei, GLsizei)

-- the speed with which things move (pixels/iteration), and their sizes(pixels)
type XoSpeed = GLsizei
type XoSize = GLsizei

-- a timepoint is a VERY important type, because it forms the basis for 
-- rollback in the simulation to the first point of collision
-- it says that, at time t, the entity was at position p, with a direction d
type TimePoint = (XoTime, XoPoint3D, XoDirection)

-- a list of these timepoints tells me the whole potential route of the entity
-- in the absence of this entity's collisions, 
-- or in the absence of other entities' collisions ending this trajectory prematurely
type TimePoints = [TimePoint]

-- the various power-ups that xonix can obtain!
-- life will give Xonix an extra life
-- grenade will kill all monsters and balls within given radius
-- turbo will speed up Xonix by given factor
-- slowtime will slow down all balls and monsters by given factor
-- all PowerUps are tied to a frame count, 
-- when the powerup is being displayed, this is a count of # of frames it will remain
-- when there is no powerup, this is a count of the # of frames before a powerup is shown
data XoPowerUp = XoLife | XoGrenade GLsizei | XoTurbo Int | XoSlowTime Int | XoOther
  deriving (Eq, Show)

-- the various shapes for an entity
data XoShape 
  = XoRectangle	{center :: XoPoint3D, width :: XoSize, height :: XoSize}
  | XoCircle 	{center :: XoPoint3D, radius :: XoSize}
  | XoDiamond 	{center :: XoPoint3D, width :: XoSize}
  | XoPolygon 	{points :: XoPoints}
  | XoTraceLine {points :: XoPoints}
  | XoLine 	{p1 :: XoPoint3D, p2 :: XoPoint3D} deriving (Show, Eq)

-- any Entity is modeled using this data
data XoEntity = XoEntity
  { shape 	:: XoShape		-- entity has a shape
  , coloring 	:: XoColor		-- and a color
  , texOfEntity :: Maybe TextureObject	-- and perhaps a texture
  , direction	:: XoDirection		-- and a direction (FIXME: should be Maybe really)
  } deriving (Show, Eq)

--the scoreboard to store score, level, and power-ups
data XoScoreBoard = XoScoreBoard
  { level 	:: Int			-- what level are we playing
  , lives	:: Int			-- how many lives are left for Xonix
  , score	:: GLsizei		-- what is the current score
  , areapc	:: GLsizei		-- what % of area is currently eaten
  , powerUp	:: (Int, Maybe (XoPoint3D, XoPowerUp))	
					-- what powerup, if any, and where, and its frame count
					-- (0, 0, 0) location indicates powerup is claimed by Xonix
					-- frame count is # of frame powerup will remain, or #frames until displayed
  , numMons	:: Int			-- how many monsters does this level start with
  , numBalls	:: Int			-- how many balls does this level start with
  , speedMult	:: Int			-- what speed multiplier does this level start with
  } deriving (Show, Eq)

-- the game is a collection of these entities
data XoGame = XoGame
  { player 	:: XoEntity		-- this is Xonix!
  , monsters 	:: [XoEntity]		-- these are the monsters
  , balls 	:: [XoEntity]		-- these are the balls
  , boundingBoxes:: [XoEntity]		-- this is the list of all the boxes
  , polyLine 	:: XoEntity		-- this is the polyLine currently active
  , scoreBd	:: XoScoreBoard		-- this is the scoreboard for the game
  } deriving (Show, Eq)

----------------------------------------------------------------------------------------
--			constants used for the game
----------------------------------------------------------------------------------------

_GAME_WINDOW_WIDTH :: XoSize
_GAME_WINDOW_WIDTH = 1000 

_GAME_WINDOW_HEIGHT :: XoSize
_GAME_WINDOW_HEIGHT = 1000

_PLAY_WINDOW_WIDTH :: XoSize
_PLAY_WINDOW_WIDTH = _GAME_WINDOW_WIDTH

_PLAY_WINDOW_HEIGHT :: XoSize
_PLAY_WINDOW_HEIGHT = _GAME_WINDOW_HEIGHT 

_NORMALIZATION_FACTOR :: XoSize
_NORMALIZATION_FACTOR = _PLAY_WINDOW_WIDTH `div` 2

--this is the amount I need to reduce to 25%
_ORIG_WIDTH :: XoSize
_ORIG_WIDTH = 800
_ORIG_HEIGHT :: XoSize
_ORIG_HEIGHT = _ORIG_WIDTH

-- this is needed for various corrections, where we perturb the position by a very small amount
-- this prevents entities from getting stuck at collision points for next frame
delta :: GLsizei
delta = 3

--computational tolerance for dynamic moving-body moving-body collision detection.
collDelta :: GLsizei
collDelta = 3

-- the time delta avoids stupid decisions due to floating point issues 
-- i.e. if the time is smaller than zeroTime, it is practically 0
-- FIXME: this should be computed using the slowest ever moving entity, which is mons@monsBaseSpeed
-- slowest speed = 2 pixel per 1 time unit ?? ?? ?? ?? ?? 
zeroTime :: Float
zeroTime = 0.001 -- lets go with this and see 

starSizeOuter :: GLsizei
starSizeOuter = 30

starSizeInner :: GLsizei
starSizeInner = 15

grenadeRange :: GLsizei
grenadeRange = 100

-- all the speeds are in pixels per 1 time unit

basePlayerSpeed :: XoSpeed
basePlayerSpeed = 4
playerRadius :: XoSize
playerRadius = 10
playerInitPosition :: XoPoint3D
playerInitPosition = (500, 950, 0)

baseMonsterSpeed :: XoSpeed
baseMonsterSpeed = (basePlayerSpeed `div` 2)
monsterRadius :: XoSize
monsterRadius = 15

baseBallSpeed :: XoSpeed
baseBallSpeed = (basePlayerSpeed * 2)
ballRadius :: XoSize
ballRadius = 12

polyLineWidth :: XoSize
polyLineWidth = 6

playArea :: XoShape 
playArea = XoRectangle (0, 0, 0) 10 10 
backgroundColor :: XoColor
backgroundColor = (0, 0, 0, 1)

--------------------------------------------------------------------------------------
--		speed utility functions
--------------------------------------------------------------------------------------

getPlayerSpeed = basePlayerSpeed

getMonsterSpeed = baseMonsterSpeed

getBallSpeed = baseBallSpeed


--------------------------------------------------------------------------------------
--		other utility functions
--------------------------------------------------------------------------------------

lenTrajec :: XoShape -> GLsizei
lenTrajec line@(XoLine p1@(p1x, p1y, p1z) p2@(p2x, p2y, p2z)) = 
  truncate $ sqrt $ fromIntegral $ (p1x-p2x)^2 + (p1y-p2y)^2 + (p1z-p2z)^2


initGame :: XoGame
initGame = XoGame (XoEntity (XoCircle (500, 950, 0) playerRadius )  (1, 0, 0, 1) Nothing (0, 0, 0))
  	          [XoEntity (XoDiamond (50, 50, 0) monsterRadius) (1, 0, 1, 1) Nothing (0, 0, 0) 
		  ]
		  [XoEntity (XoCircle (700, 500, 0) ballRadius) (0, 1, 0, 1) Nothing (1, 1, 0)
		  ,XoEntity (XoCircle (500, 500, 0) ballRadius) (0, 1, 0, 1) Nothing (-1, -1, 0)
		  ]
		  [originalBoundingBox]
		  (XoEntity (XoTraceLine []) (1, 1, 1, 1) Nothing (0, 0, 0) )
		  initScoreBoard

pupAppearCount :: Int
pupAppearCount = (1500) -- #frames for PowerUp to appear

pupRemainCount :: Int
pupRemainCount = (360 * 2) -- #frames for PowerUp to remain

initScoreBoard :: XoScoreBoard
initScoreBoard = XoScoreBoard {level = 1, lives = 3, score = 0, areapc = 0, powerUp = (pupAppearCount, Nothing), numMons = 1, numBalls = 2, speedMult = 1}

--placement of the original bounding box on a new level start
originalBoundingBox = XoEntity (XoPolygon [(100, 100, 0), (900, 100, 0), (900, 900, 0), (100, 900, 0)]) (0, 1, 1, 1) Nothing (0, 0, 0)

--the coordinates between which balls are placed on a new level start
ballsPlacementList :: [(XoPoint3D, XoPoint3D)]
ballsPlacementList = ((120, 120, 0), (880, 880, 0)) : ballsPlacementList
exampleBall = XoEntity (XoCircle (100, 750, 0) ballRadius) (0, 1, 0, 1) Nothing (-1, -1, 0)

-- the list of boxes (lower-left, upper-right) in which monsters are placed at the start of a level, or on player death
monsPlacementList :: [(XoPoint3D, XoPoint3D)]
monsPlacementList = ((20, 20, 0), (80, 980, 0)) : ((120, 20, 0), (880, 80, 0)) : ((920, 20, 0), (980, 980, 0)) : monsPlacementList	 
exampleMonster = XoEntity (XoDiamond (50, 50, 0) monsterRadius) (1, 0, 1, 1) Nothing (0, 0, 0) 



