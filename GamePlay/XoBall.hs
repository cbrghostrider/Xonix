--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoBall (evaluateBall) where

import XoData
import XoCollision
import XoBoundingBox

--------------------------------------------------------------------------------------
--			Evaluation for ball
--------------------------------------------------------------------------------------

evaluateBall :: XoGame -> XoTime -> XoEntity -> TimePoints
evaluateBall g frameTime b@(XoEntity origCir@(XoCircle cen@(xc, yc, zc) rad) col tex dir@(xd, yd, zd)) 
 = (time, (center $ shape newB'), (direction newB')) : futureTimePoints
  where
    -- baseComponent tells me how fast the ball is travelling along an x or y parallel line 
    -- (getBallSpeed is the ball's speed along the 45 degree line)
    baseComponent = truncate $ frameTime * sqrt (fromIntegral (getBallSpeed*getBallSpeed) / 2)
    rad'   = truncate $ sqrt (fromIntegral (rad*rad) / 2)

    --get the new center of the ball, if there was no collision
    newC@(nX, nY, nZ) = (xc + xd*baseComponent, yc + yd*baseComponent, 0)
    newC'@(nX', nY', nZ') = (nX + xd*rad', nY + yd*rad', 0) -- test against projected forward by ballradius

    --get any collision info if it happens, so that we can rebound the ball
    collisionInfo = getBallCollision g b newC'
    newCircle = XoCircle newC rad 

    --rebound the ball
    newB = if (collisionInfo /= Nothing) 
	      then getNewBallOnCollision g b newC' collisionInfo
	      else b{shape = newCircle}

    --and perform edge correction so that ball doesn't look like it went through the wall
    newB' = if (collisionInfo /= Nothing)
	      then performEdgeCorrection newB dir
	      else newB

    --now compute how long it took for the ball to move from prior center to this point, 
    ballTrajec = XoLine cen $ center $ shape newB'
    unhinderedTrajec = XoLine cen newC
    potentialTime = (fromIntegral $ lenTrajec ballTrajec) / (fromIntegral $ lenTrajec unhinderedTrajec)
    time = if (lenTrajec unhinderedTrajec == 0 || frameTime - potentialTime <= zeroTime) 
		then frameTime 
		else potentialTime
    
    --compute recursive position if needed, else end here
    futureTimePoints = if (frameTime - time < zeroTime) then [] else evaluateBall g (frameTime - time) newB'

--edge correction makes the ball bounce at the wall, rather than after getting into it
performEdgeCorrection :: XoEntity -> XoDirection -> XoEntity
performEdgeCorrection b@(XoEntity origCir@(XoCircle cen@(xc, yc, zc) rad) col tex dir@(xd, yd, zd)) oldDirection = b{shape = newCircle}
  where
    (odx, ody, odz) = oldDirection
    xc' = xc + odx*(-1)*rad
    yc' = yc + ody*(-1)*rad
    cen' = (xc', yc', 0)
    newCircle = origCir {center = cen'} 

performEdgeCorrection _ _ = error "Unexpected call to performEdgeCorrection"

--gives the new placement of the ball when collision is detected
getNewBallOnCollision :: XoGame -> XoEntity -> XoPoint3D -> Maybe (XoShape, XoPoint3D) -> XoEntity
getNewBallOnCollision g b@(XoEntity origCir@(XoCircle cen@(xc, yc, zc) rad) col tex dir@(xd, yd, zd)) newCenter (Just (collWithLine@(XoLine p1 p2), collPt)) = newB
  where
    newC' = collPt
    newDir = newDirectionForBall collWithLine newC' dir
    newCir = XoCircle newC' rad
    newB = b {shape = newCir, direction = newDir}
    
getNewBallOnCollision _ _ _ _ = error "unexpected call to find ball collision in collision detection"

-- to compute Collision-Line and Collision-Point for ball-edge collision
getBallCollision :: XoGame -> XoEntity -> XoPoint3D -> Maybe (XoShape, XoPoint3D)
getBallCollision g b@(XoEntity origCir@(XoCircle cen@(xc, yc, zc) rad) col tex dir@(xd, yd, zd)) newC = collision
    where
      bbEdges = concat $ map getListOfBoundingBoxEdges $ boundingBoxes g
      ballTrajec = XoLine cen newC
      collideBB = zip bbEdges $ map (intersectPoint ballTrajec) $ map (\(p1, p2) -> XoLine p1 p2) bbEdges
      collision = getCollidingEdgeAndPoint collideBB cen

getBallCollision _ _ _ = error "Ball collision detection error"

-- to compute the new direction vector for ball on collision
newDirectionForBall :: XoShape -> XoPoint3D -> XoDirection -> XoDirection
newDirectionForBall collLine@(XoLine p1@(p1x, p1y, p1z) p2@(p2x, p2y, p2z)) collPt@(cx, cy, cz) oldDir@(odx, ody, odz) 
  | p1x==p2x = ((-1)*odx, ody, odz)
  | p1y==p2y = (odx, (-1)*ody, odz)
  | otherwise = error "Unknown orientation of CollisionLine (non x/y parallel) with bouncing object"

newDirectionForBall _ _ _ = error "Unexpected call for newDirectionForBall"



