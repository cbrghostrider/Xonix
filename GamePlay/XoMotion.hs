--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoMotion (idleFunc) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import XoData
import XoBoundingBox
import XoCollision
import XoPlayer
import XoBall
import XoMonster
import XoPolyLine
import XoRandom
import XoPowerUp
import XoAudio

import Data.List 

addInitTimePoint :: XoEntity -> TimePoints -> TimePoints
addInitTimePoint e tps = (0, center (shape e), direction e) : tps

-- the idle function will evaluate the motion of the various entities
-- as per the algorithm listed in the docs section of the svn
idleFunc :: XoGame -> IO XoGame
idleFunc g = do 
  let (newGame, didCollide, soundIndication) = idleFunc' g
  if soundIndication /= Nothing then let Just (soundEvent) = soundIndication in xonixSoundEvent soundEvent
				else return ()
  newGame' <- if didCollide then if (lives $ scoreBd newGame) <= 0   then return newGame --error "No more lives! Game ends!"
								     else genGamePlayerDeath newGame
			    else if (areapc $ scoreBd newGame) >= 75 then genGameNewLevel newGame
			    					     else return newGame
  pup <- processPowerUpCountdown newGame'
  return (newGame'{scoreBd = ((scoreBd newGame'){powerUp = pup})})

idleFunc' :: XoGame -> (XoGame, Bool, Maybe XoSoundEvent)
idleFunc' g = (gNew, (earliestCollision /= Nothing), soundEvent)
  where
    -----------------------------------------------------------------------------------------
    --         Phase - 1: the player is evaluated for max frameTime and max player movement
    -----------------------------------------------------------------------------------------
    -- ::(XoPoint3D, XoTime, Maybe(XoLine, XoPoint3D))
    (playerCenNew, frameTime, playerColl) = evaluatePlayer g $ player g

    -- if playerColl has happened, then a polyLine event has taken place, 
    -- and the time for this event to happen is frameTime 
    polySpecificGame = 
      if playerColl == Nothing then g 
  			       else let Just(collEdge, collPt) = playerColl 
				    in  evaluatePolyEdgeEvent g ((player g){shape = ((shape $ player g){center = playerCenNew})}) (collEdge, collPt)
    soundEvent = Nothing{-if playerColl == Nothing then Nothing
					  else if (polyLine g == []) then Just XoPolyStart else Just XoPolyStop -}
    potentialBoundingBoxes = boundingBoxes polySpecificGame
    potentialPolyLine = polyLine polySpecificGame

    -----------------------------------------------------------------------------------------
    --         Phase - 2: the non-player moving entities are also evaluated independently
    -----------------------------------------------------------------------------------------
    --evaluate the balls
    newBallTimePointss = map (\(ball, tps) -> addInitTimePoint ball tps) $ zip (balls g) $ map (evaluateBall g frameTime) $ balls g

    --evaluate the monsters
    newMonsterTimePointss = map (\(mons, tp) -> addInitTimePoint mons [tp]) $ zip (monsters g) $ map (evaluateMons g frameTime) $ monsters g

    -----------------------------------------------------------------------------------------
    --         Phase - 3: Evaluate any moving body - moving body collision detection
    -----------------------------------------------------------------------------------------
    playerMove = ((center.shape.player) g, playerCenNew)

    ballPlayerCollisions = map (determineEarliestDynamicCollision g frameTime (playerRadius + ballRadius) playerMove) $ newBallTimePointss

    monsPlayerCollisions = map (determineEarliestDynamicCollision g frameTime (playerRadius + monsterRadius) playerMove) $ newMonsterTimePointss

    -----------------------------------------------------------------------------------------
    --         Phase - 4: Evaluate the earliest collision of balls or player w/ polyLine
    -----------------------------------------------------------------------------------------
    ballPolyCollisions = map (determineEarliestPolyCollision g frameTime (ballRadius + polyLineWidth `div` 2) ((center $ shape $ player g), playerCenNew) False) 
				$ newBallTimePointss

    playerTimePoints = (0, center (shape (player g)), direction (player g)):[(frameTime, playerCenNew, direction (player g))]

    playerPolyCollisions = map (determineEarliestPolyCollision g frameTime (playerRadius + polyLineWidth `div` 2) ((center $ shape $ player g), playerCenNew) True)
				$ [playerTimePoints]

    -----------------------------------------------------------------------------------------
    --   Phase - 5: compute the earliest collision OF ALL collisions that have happened
    -----------------------------------------------------------------------------------------
    allCollisions = filter (\x -> x/=Nothing) $ ballPlayerCollisions ++ monsPlayerCollisions ++ ballPolyCollisions ++ playerPolyCollisions
    
    earliestCollision = if (null allCollisions) 
			  then Nothing 
			  else head $ sortBy (\(Just (time1, pt1)) (Just (time2, point2)) -> 
						if (time1 > time2) then GT else if (time1 < time2) then LT else EQ) allCollisions 

    -----------------------------------------------------------------------------------------
    --         Phase - 6: compute final resting places of all the entities;
    --	   and now that we have all the information, lets update the scoreboard too
    -----------------------------------------------------------------------------------------

    newFrameTime = if ((earliestCollision) == Nothing) then frameTime else let Just (time, _) = earliestCollision in time
    newPlayer = advanceEntity newFrameTime (player g) playerTimePoints

    newBalls = map (\(ball, ballTimePoints) -> advanceEntity newFrameTime ball ballTimePoints) $ zip (balls g) newBallTimePointss
    newMonsters = map (\(mons, monsTimePoints) -> advanceEntity newFrameTime mons monsTimePoints) $ zip (monsters g) newMonsterTimePointss

    --first let's get the score to add
    scoreToAdd = ((sum $ map computeAreaOfBB $ map shape $ filter (isEmptyBoundingBox g) $ potentialBoundingBoxes) * 100 ) `div` (_ORIG_WIDTH * _ORIG_HEIGHT)

    --given the new position of the balls (which could have passed the traceline *before* it was drawn) 
    --we now compute the final game by eliminating empty bounding boxes (considering *new* ball positions)
    newBoundingBoxes = map fst $ filter (\(_, x) -> x == False) $ zip potentialBoundingBoxes $ map (isEmptyBoundingBox g) $ potentialBoundingBoxes 

    newScore = (score $ scoreBd g) + scoreToAdd
    newArea =  (((_ORIG_WIDTH * _ORIG_HEIGHT) - (sum $ map computeAreaOfBB $ map shape $ newBoundingBoxes)) * 100 ) `div` (_ORIG_WIDTH * _ORIG_HEIGHT)
    newScoreBoard = (scoreBd g) {score=newScore, areapc = newArea, powerUp = (powerUp $ scoreBd g)}

    --finally we can compute the new game
    gNew = g { player = newPlayer
	     , monsters = newMonsters
	     , balls = newBalls
	     , boundingBoxes = newBoundingBoxes
	     , polyLine = potentialPolyLine
	     , scoreBd = newScoreBoard
	     }

--------------------------------------------------------------------------------------------------------
--			Misc utility functions
--------------------------------------------------------------------------------------------------------

--given an entity, new frametime, and all the TimePoints for that entity
--this function advances the entity to the appropriate final place
advanceEntity :: XoTime -> XoEntity -> TimePoints -> XoEntity
advanceEntity newTime ent ((tp1@(t1, p1@(p1x, p1y, p1z), d1)):(tp2@(t2, p2@(p2x, p2y, p2z), d2)):tps)
  | newTime > t2 = advanceEntity (newTime - t2) ent (tp2:tps)
  | otherwise = ent {shape = newShapeOther, direction=d1}
    where
      oldCenter@(ocx, ocy, ocz) = center $ shape $ ent
      (lx, ly, lz) = (p2x-p1x, p2y-p1y, p2z-p1z)
      ratio = if (t2 - newTime < zeroTime) then 1.0 else newTime/t2
      deltax = truncate $ fromIntegral lx*ratio
      deltay = truncate $ fromIntegral ly*ratio
      newCenterOther = (ocx + deltax, ocy + deltay, ocz)
      newShapeOther = (shape ent){center = newCenterOther}
      newEntityOther = ent{shape = newShapeOther, direction=d1}
      
advanceEntity newTime ent ((tp1@(t1, p1, d1)):[]) = ent -- FIXME this really leads to error "Not enough time points to advance entity"

advanceEntity _ _ _ = error "Unknown shape, or other error in advance entity"


-- called when we need a new level
genGameNewLevel = genGame True

-- called when the player dies
genGamePlayerDeath = genGame False

-- function to generate a game, given the information in the scoreboard
-- this will either generate a brand new game with 1 BB, and mons and balls as specified
-- or it will gen a partial game (with BB and balls at the same place) but player and monsters at init place 
-- either way, new game will have no polyline
genGame :: Bool -> XoGame -> IO XoGame 
genGame initAll g = do
    let scoreBdNew 	= (scoreBd g){ level 	= if initAll then (level $ scoreBd g) + 1 else (level $ scoreBd g)
				     , lives 	= if initAll then (lives $ scoreBd g) else (lives $ scoreBd g) - 1
				     , score 	= (score $ scoreBd g)
				     , areapc 	= if initAll then 0 else (areapc $ scoreBd g)
				     , powerUp 	= (pupAppearCount, Nothing) 
				     , numMons 	= if initAll then 1 + (((level $ scoreBd g) + 1 + 1) `div` 3) else (numMons $ scoreBd g)
				     , numBalls = if initAll then 1 + (((level $ scoreBd g) + 1 + 2) `div` 3) else (numBalls $ scoreBd g)
				     , speedMult= if initAll then 1 + (((level $ scoreBd g) + 1 + 0) `div` 3) else (speedMult $ scoreBd g)
				     } 
    let playerNew	= (player g){shape = ((shape $ player g){center = playerInitPosition}), direction = (0, 0, 0)}
    let numMonsToGen	= if initAll then (numMons $ scoreBdNew) else (length $ monsters g)
    monstersNew         <- mapM (\placeWithin -> do
					cen <- getRandomPlacement placeWithin
					return (exampleMonster{shape = ((shape exampleMonster){center = cen})})) (take numMonsToGen monsPlacementList)
    ballsNewIfNeeded    <- mapM (\placeWithin -> do
					cen <- getRandomPlacement placeWithin
					dir <- getRandomBallDirection
					return (exampleBall {shape = ((shape exampleBall){center = cen}), direction = dir})) (take (numBalls scoreBdNew) ballsPlacementList) 
    let ballsNew 	= if initAll then ballsNewIfNeeded else balls g
    let bbNew 		= if initAll then [originalBoundingBox] else boundingBoxes g
    let polyNew		= (polyLine g){shape = ((shape $ polyLine g){points = []})}
    let gNew 		= g{ player = playerNew
			   , monsters = monstersNew
			   , balls = ballsNew
			   , boundingBoxes = bbNew
			   , polyLine = polyNew
			   , scoreBd = scoreBdNew}
    return gNew

