--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoMonster (evaluateMons) where

import XoData
import XoCollision
import XoBoundingBox

--------------------------------------------------------------------------------------
--			Evaluation for monsters
--------------------------------------------------------------------------------------

-- this wil take in the old game, and the old monster location
-- along with the time for which the monster can move (maximum)
-- it will return, the new time (same as old, along with the new center location of the monster as a point)
evaluateMons :: XoGame -> XoTime -> XoEntity -> TimePoint 
evaluateMons g frameTime m@(XoEntity shp@(XoDiamond cen@(cx, cy, cz) width) col tex dir@(dx, dy, dz)) = (frameTime, locat, newDir)
-- = m{shape = newShape, direction = newDir}
  where
    plShp@(XoCircle plCen@(plcx, plcy, plcz) plRad) = shape $ player g
    newDir@(ndx, ndy, ndz) = (plcx-cx, plcy-cy, plcz-cz)
    mag = ceiling $ sqrt $ fromIntegral $ndx*ndx + ndy*ndy + 0
    newC@(ncx, ncy, ncz) = if (mag == 0) then cen 
	     				 else ((cx+ ceiling (frameTime * fromIntegral (getMonsterSpeed*ndx) /fromIntegral mag)),
	           			       (cy+ ceiling (frameTime * fromIntegral (getMonsterSpeed*ndy) /fromIntegral mag)),
				               (0))
    rad' = ceiling $ (sqrt $ 2.0 * fromIntegral width * fromIntegral width) / 2.0 
    monsTrajec = XoLine cen newC

    collideBB = getCollideBB monsTrajec $ growBoundingBoxEdges rad' $ concat $ map getListOfBoundingBoxEdges $ boundingBoxes g
    collisionInfo = getCollidingEdgeAndPoint collideBB cen 
    
    locat = if (collisionInfo /= Nothing)
		then let Just (collEdge@(XoLine p1@(p1x, p1y, p1z) p2@(p2x, p2y, p2z)), collPt@(collx, colly, collz)) = collisionInfo
			 finalx = if p1x == p2x then if p1y < p2y then collx + delta else collx - delta else collx
			 finaly = if p1y == p2y then if p1x < p2x then colly - delta else colly + delta else colly
		     in (finalx, finaly, 0)
		else newC 
    newShape = XoDiamond locat width

evaluateMons _ _ _ = error "Evaluate monster called for non-diamond shape"



