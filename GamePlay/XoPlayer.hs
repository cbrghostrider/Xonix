-- -------------------------------------------------------------------------------------
--         Author: Sourabh S Joshi (cbrghostrider); Copyright - All rights reserved.
--                       For email, run on linux (perl v5.8.5):
--   perl -e 'print pack "H*","736f75726162682e732e6a6f73686940676d61696c2e636f6d0a"'
-- -------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoPlayer (evaluatePlayer) where

import XoData
import XoCollision
import XoBoundingBox 

--------------------------------------------------------------------------------------
--			Evaluation for player
--------------------------------------------------------------------------------------

-- given a game, and a player old position, this will return
-- new player position as a center point, time it took to get there, and a possible collisionInfo pair
-- it returns a new player position as a point, because later on during simulation, we may find that
-- the player actually gets killed before getting to the final point
evaluatePlayer :: XoGame -> XoEntity -> (XoPoint3D, XoTime, Maybe (XoShape, XoPoint3D))
evaluatePlayer g p@(XoEntity (XoCircle cen@(xc, yc, zc) rad) col tex dir@(xd, yd, zd)) = (newCen'', time, collisionInfo)
   where
    --don't need actual vectors, since player moves only in +y/-y/+x/-x
    --first compute how much distance the player will travel in 1 time unit
    (nX, nY, nZ) = (xc + xd*getPlayerSpeed, yc + yd*getPlayerSpeed, 0)
    nX' = if (nX >= _GAME_WINDOW_WIDTH - playerRadius) then (_GAME_WINDOW_WIDTH-playerRadius) 
						       else if (nX <= playerRadius) then (playerRadius) else nX
    nY' = if (nY >= _GAME_WINDOW_WIDTH - playerRadius) then (_GAME_WINDOW_WIDTH-playerRadius) 
						       else if (nY <= playerRadius) then (playerRadius) else nY
    newCen = (nX', nY', 0)
    -- then compute if the player is starting or ending a polyline
    -- if so, the time is clipped to this earlier time.
    playerTrajec = XoLine cen newCen
    collideBB = getCollideBB playerTrajec $ concat $ map getListOfBoundingBoxEdges $ boundingBoxes g
    collisionInfo = getCollidingEdgeAndPoint collideBB cen

    --get the potential new center of the player
    newCen'@(ncx', ncy', ncz') = if (collisionInfo /= Nothing) then let Just (collEdge, collPt) = collisionInfo in collPt else newCen 
    --now perturb the newCen if there was a collision, so that the player is just beyond the collision point
    newCen'' = if (collisionInfo/= Nothing) 
		 then let Just (collEdge@(XoLine p1@(p1x, p1y, p1z) p2@(p2x, p2y, p2z)), collPt@(collx, colly, collz)) = collisionInfo
		          finalx = if (xd == 1) then collx + delta else if xd == -1 then collx - delta else collx
		          finaly = if (yd == 1) then colly + delta else if yd == -1 then colly - delta else colly
		      in  (finalx, finaly, collz)
		 else newCen' 
					
    --get the time to be returned (if player did not move, return 1)
    time = if (lenTrajec playerTrajec == 0) then 1.0 else (fromIntegral $ lenTrajec $ XoLine cen newCen'')/(fromIntegral $ lenTrajec $ playerTrajec)

    --get the final placement of the player 
    --newCircle = (shape p){center = newCen''}

evaluatePlayer _ _ = error "Found non-circle player in evaluatePlayer!"    


