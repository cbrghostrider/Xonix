--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoRandom (initRandomizer, getRandomPlacement, getRandomBallDirection, getRandomPup) where

import XoData
import System.Random
import Graphics.Rendering.OpenGL


seed :: Int
seed = 101001

initRandomizer :: IO ()
initRandomizer = do
  setStdGen $ mkStdGen seed
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  _ <- getRandomBallDirection
  return ()

--this function takes a rectangle (lower left, upper right) as input
--and gives a point within the rectangle at random
--this is used to place the balls initially
--and also used to place the monsters when the player dies, or initially
getRandomPlacement :: (XoPoint3D, XoPoint3D) -> IO XoPoint3D
getRandomPlacement (ll@(llx, lly, _), ur@(urx, ury, _)) = do
  x <- if llx >= urx then return llx else getRandom (llx, urx)
  y <- if lly >= ury then return llx else getRandom (lly, ury)
  let z = 0
  return (x, y, z)

-- this function is used to get a random direction for the ball (+/-1, +/-1, 0)
-- so that we can init the ball direction
getRandomBallDirection :: IO (XoPoint3D)
getRandomBallDirection = do
  gen <- newStdGen
  let (rlow, rhigh) = genRange gen
  let (rawNumx, gen') = next gen
  let xd = if (rawNumx < (rhigh `div` 2)) then -1 else 1
  let (rawNumy, gen'') = next gen'
  let yd = if (rawNumy < (rhigh `div` 2)) then -1 else 1
  return (xd, yd, 0)

-- this function will return a random powerUp type
getRandomPup :: IO XoPowerUp
getRandomPup = do
  gen <- newStdGen
  let (rawNum, gen') = next gen
  if rawNum `mod` 2 == 0 then return (XoLife) 
			 else return (XoGrenade grenadeRange)

-- given a range of integers (non-zero numbers) 
-- it will return a random integer within that range
getRandom :: (GLsizei, GLsizei) -> IO (GLsizei)
getRandom (low, high) = do
  gen <- newStdGen
  let (rawNum, gen') = next gen
  let newNum = ((fromIntegral rawNum `mod` (high-low)) + low)
  return newNum



