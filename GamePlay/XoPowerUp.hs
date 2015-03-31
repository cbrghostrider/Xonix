--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoPowerUp (processPowerUpCountdown, consumePowerUp) where

import Graphics.Rendering.OpenGL
import Data.List

import XoData
import XoRandom
import XoBoundingBox

-- a position of (0, 0, 0) with a powerup indicates xonix currently has powerup
-- the framecount is a count of either # of frames to keep powerup in current place
-- 			or # of frames before powerup will appear
processPowerUpCountdown :: XoGame -> IO (Int, Maybe (XoPoint3D, XoPowerUp))
processPowerUpCountdown g = do
  let (currTime, currPup) = (powerUp $ scoreBd g)
  if currPup /= Nothing
      then let Just (pupPlace, pupType) = currPup
	       (xp, yp, zp) = pupPlace
	   in if xp == 0 && yp == 0 && zp == 0
		then return (currTime, currPup) --PowerUp is primed for use by Xonix
		else if currTime <= 0 
			then return (pupAppearCount, Nothing) -- PowerUp wasn't captured by xonix in amount
			 				      -- of time available... it will go away now.
		 	else let dist = distanceBetween pupPlace (center $ shape $ player g) 
			     in	 if dist > (starSizeOuter `div` 2) + playerRadius 
					then return (currTime - 1, currPup)  -- xonix still has time to try to capture this
					else return (currTime, Just((0, 0, 0), pupType)) -- xonix got powerup!
      else if currTime > 0 
		then return (currTime - 1, Nothing) -- still to count down some more before powerup appearing
		else do
			pupPlace' <- getRandomPlacement ((30, 30, 0), (970, 970, 0))
			pupType' <- getRandomPup 
			return (pupRemainCount, Just(pupPlace', pupType')) -- generate new PUP for xonix

-- when the user presses space to consume powerup, 
-- this will do the needful    
consumePowerUp :: XoGame -> XoGame
consumePowerUp g = newG
  where
    (_, powerup) = powerUp $ scoreBd g
    newG = if powerup == Nothing 
		then g
		else let Just(place@(px, py, pz), pupType) = powerup 
			 prevLives = lives $ scoreBd g
		     in  if (px == 0 && py == 0 && pz == 0) 
				then case pupType of 
					XoLife 		-> g{scoreBd = ((scoreBd g){lives = prevLives + 1, powerUp = (pupAppearCount, Nothing)})}
					XoGrenade r	-> activateGrenade g r
					_		-> error "Do not know how to consume powerup yet!"
				     
				else g

activateGrenade :: XoGame -> GLsizei -> XoGame
activateGrenade g r = g{monsters = newMons, balls = newBalls, boundingBoxes = newBB, polyLine = newPoly, scoreBd = newSc}
  where
    --find out which monsters and balls to destroy
    delMons  = map fst $ filter (\(m, d) -> d < r) $ zip (monsters g) $ map (distanceBetween (center $ shape $ player g)) $ map (center.shape) $ monsters g
    delBalls = map fst $ filter (\(b, d) -> d < r) $ zip (balls g)    $ map (distanceBetween (center $ shape $ player g)) $ map (center.shape) $ balls g
    --and destroy those
    newMons = (monsters g) \\ delMons
    newBalls = (balls g) \\ delBalls 
    --we lost the powerup on use, so update scoreboard
    newSc = (scoreBd g){powerUp = (pupAppearCount, Nothing)}
    --and if we happened to cause some bounding boxes to become empty (and possibly the one xonix is in)
    --then we need to remove the BBs and possibly the polyline
    playerBoundingBox = map fst $ filter (\(_, v) -> v == True) $  zip (boundingBoxes g) $ map (\x -> isPointInsideBoundingBox x (center $ shape $ player g)) $ boundingBoxes g
    newPoly = if null playerBoundingBox 
		 then (polyLine g)
		 else if (isEmptyBoundingBox (g{balls = newBalls}) (head playerBoundingBox) )
			then (polyLine g){shape = ((shape $ polyLine g){points = []})}
			else (polyLine g)
    newBB = if (not $ null playerBoundingBox) && (isEmptyBoundingBox (g{balls=newBalls}) (head playerBoundingBox))
		then (boundingBoxes g) \\ [(head $ playerBoundingBox)]
		else boundingBoxes g
    --note: removal of the other collateral damage bounding boxes isn't really needed, since it will be done on next round
    --(i.e. grenade could have killed a ball in the adjacent bounding box, if it was within blast radius, 
    --thus making more than 1 bounding box empty)... but this is handled in idlefunc anyway
			
		

distanceBetween :: XoPoint3D -> XoPoint3D -> GLsizei
distanceBetween p0@(x0, y0, z0) p1@(x1, y1, z1) = truncate $ sqrt $ fromIntegral $ (x1-x0)^2 + (y1-y0)^2 + (z1-z0)^2


