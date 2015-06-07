-- -------------------------------------------------------------------------------------
--         Author: Sourabh S Joshi (cbrghostrider); Copyright - All rights reserved.
--                       For email, run on linux (perl v5.8.5):
--   perl -e 'print pack "H*","736f75726162682e732e6a6f73686940676d61696c2e636f6d0a"'
-- -------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoCollision (getCollidingEdgeAndPoint, intersectPoint, 
		    determineEarliestDynamicCollision, determineEarliestPolyCollision) where
import XoData
import Data.List

--------------------------------------------------------------------------------------
--		Collision detection between two lines
--------------------------------------------------------------------------------------

getCollidingEdgeAndPoint :: [((XoPoint3D, XoPoint3D), Maybe XoPoint3D)] -> XoPoint3D -> Maybe (XoShape, XoPoint3D)
getCollidingEdgeAndPoint collideBB oldLoc@(ox, oy, oz) = collision
  where
    collisionCandidates = filter (\(_,x) -> x /= Nothing) collideBB
    distancedCandidates = zip (map (\ (_, Just colPt@(cx, cy, cz)) -> ((cx-ox)^2) + ((cy-oy)^2) + ((cz-oz)^2)) collisionCandidates) collisionCandidates
    sortedCandidates = map snd $ sortBy (\(da2, _) (db2, _) -> if (da2 > db2) then GT else if (da2 < db2) then LT else EQ ) distancedCandidates
    collision = 
      if (not(null collisionCandidates)) 
	then Just ((\(x, y) -> XoLine x y) (p1, p2), pos)
	else Nothing
    ((p1, p2), (Just pos)) = head sortedCandidates

intersectPoint :: XoShape -> XoShape -> Maybe XoPoint3D
intersectPoint (XoLine p1l1 p2l1) (XoLine p1l2 p2l2)
  | (p1l1 == p2l1 ) || (p1l2 == p2l2)= Nothing
  | d == 0 && collinearX && (  ((y1 `elem` (take 2 sortedY)) && (y2 `elem` (drop 2 sortedY)) ) 
			    || ((y2 `elem` (take 2 sortedY)) && (y1 `elem` (drop 2 sortedY)))  ) = Just p2l1
  | d == 0 && collinearY && (  ((x1 `elem` (take 2 sortedX)) && (x2 `elem` (drop 2 sortedX)) ) 
			    || ((x2 `elem` (take 2 sortedX)) && (x1 `elem` (drop 2 sortedX)))  ) = Just p2l1
  | d == 0  = Nothing 
  | (onLineSegment l1 intPt) && (onLineSegment l2 intPt) = Just intPt 
  | otherwise = Nothing
    where 
	  l1 = XoLine p1l1 p2l1
	  l2 = XoLine p1l2 p2l2
          (x1, y1, z1) = p1l1
          (x2, y2, z2) = p2l1
          (x3, y3, z3) = p1l2
          (x4, y4, z4) = p2l2
          d = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
          nx= (x1*y2 - y1*x2)*(x3-x4) - (x1-x2)*(x3*y4 - y3*x4)
          ny= (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4 - y3*x4)
	  px = nx `div` d
	  py = ny `div` d
	  intPt = (px, py, 0)
  	  collinearX = x1==x2 && x2==x3 && x3==x4
	  collinearY = y1==y2 && y2==y3 && y3==y4
	  sortedY = sort [y1, y2, y3, y4]
	  sortedX = sort [x1, x2, x3, x4]

intersectPoint _ _ = error "Line Intersection algorithm called for non-line shape"

onLineSegment :: XoShape -> XoPoint3D -> Bool
onLineSegment (XoLine ls1 ls2) pTest@(tx, ty, _) 
  | (xa <= xb && ya <= yb) = (xa <= tx && tx <= xb) && (ya <= ty && ty <= yb)
  | (xa <= xb && ya >= yb) = (xa <= tx && tx <= xb) && (yb <= ty && ty <= ya)
  | (xa >= xb && ya <= yb) = (xb <= tx && tx <= xa) && (ya <= ty && ty <= yb)
  | (xa >= xb && ya >= yb) = (xb <= tx && tx <= xa) && (yb <= ty && ty <= ya)
  | otherwise = False
    where
      (xa, ya, za) = ls1
      (xb, yb, zb) = ls2

onLineSegment _ _ = error "Line Segment test called for non-line shape"

-------------------------------------------------------------------------------------
--		Collision detection between moving and static entities 
--		e.g. (player and poly) or (ball and poly)
-------------------------------------------------------------------------------------
determineEarliestPolyCollision :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> Bool -> TimePoints -> Maybe (XoTime, XoPoint3D)
determineEarliestPolyCollision g frameTime collisionTolerance plmove@(plOld, plNew) amIPlayer (tp1@(t1, p1@(p1x, p1y, p1z), _) : tp2@(t2, p2@(p2x, p2y, p2z), _) : tps)
  | null (points $ shape $ polyLine g) = Nothing
  | otherwise = if (null candidates') then Nothing else head orderedCandidates
  where
    --the list of all pts along which to check is
    --list of original pts in the poly - this forms the static checks
    --segment from last pt in poly to old player position - this is also a static check, since the player was already present there
    --segmenet from last player center to this player center - this is a dynamic check, since player will be moving along this in this frame
    listPolyPts = (points $ shape $ polyLine g) ++ [(plOld)] ++ [(plNew)] -- at least 3 points
    listPolySegs = map (\(p1, p2) -> XoLine p1 p2) $ zip listPolyPts (tail listPolyPts)
    -- temp FIXME listTestSeg = XoLine ?? 
    staticList = init listPolySegs   -- guaranteed to exist
    dynamicList = last listPolySegs  -- guaranteed to exist

    --all balls will be checked against both static and dynamic lists
    --player will be checked only against the static list 
    --(for player ignore last collision at plOld in static list as well, by pruning the static list)
    staticList' = if amIPlayer then init staticList else staticList
    dynamicList' = if amIPlayer then [] else [dynamicList]

    --FIXME: Generally Ignoring trajectories that come very close to each other but do not touch :-(
    collideSegmentInfoStatic  = map (checkEntityAgainstStaticPolySegment g frameTime collisionTolerance plmove amIPlayer (tp1:tp2:tps)) staticList'
    --collideSegmentInfoDynamic = map (checkEntityAgainstStaticPolySegment g frameTime collisionTolerance plmove amIPlayer (tp1:tp2:tps)) dynamicList'
    --FIXME: for the dynamic collision, need to actually make sure the poly is grown when ball strikes it!
    --BIG HACK, temporarily set to null
    collideSegmentInfoDynamic = [Nothing] 
    candidates = filter (/=Nothing) (collideSegmentInfoStatic ++ collideSegmentInfoDynamic)
    candidates' = if (not amIPlayer) then candidates else filter (\(Just(_, x)) -> x /= plOld) $ filter(\(Just(_, x)) -> x /=  plNew) candidates
    orderedCandidates = sortBy (\ (Just(t1, _)) (Just(t2, _)) -> if t1<t2 then LT else if t1>t2 then GT else EQ) candidates'
  
determineEarliestStaticCollision _ _ _ _ _ [] 		= error "Need at least 2 timepoints for detecting collisions"
determineEarliestStaticCollision _ _ _ _ _ (tp1:[]) 	= error "Need at least 2 timepoints for detecting collision"


checkEntityAgainstStaticPolySegment :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> Bool -> TimePoints -> XoShape-> Maybe (XoTime, XoPoint3D)
checkEntityAgainstStaticPolySegment _ _ _ _ _ [] _ = Nothing
checkEntityAgainstStaticPolySegment _ _ _ _ _ (tp:[]) _ = Nothing
checkEntityAgainstStaticPolySegment g frameTime collisionTolerance plmove@(plOld, plNew) amIPlayer (tp1@(t1, p1@(p1x, p1y, p1z), d1@(d1x, d1y, d1z)) : tp2@(t2, p2@(p2x, p2y, p2z), _) : tps) checkSegment = final
  where
    mag = sqrt $ fromIntegral ((d1x)^2 + (d1y)^2 + (d1z)^2)
    deltax = truncate $ fromIntegral d1x * fromIntegral collisionTolerance / mag
    deltay = truncate $ fromIntegral d1y * fromIntegral collisionTolerance / mag
    p2' = (p2x + deltax, p2y + deltay, p2z)
    collisionInfo = intersectPoint checkSegment (XoLine p1 p2')
    nextLevelInfo = checkEntityAgainstStaticPolySegment g (frameTime - t2) collisionTolerance plmove amIPlayer (tp2:tps) checkSegment
    timeFinal = if collisionInfo == Nothing then if nextLevelInfo == Nothing then 0 -- doesn't really matter
									     else let Just (tNext, collNext) = nextLevelInfo in (t2 + tNext)
					    else let Just (collPt@(cx, cy, cz)) = collisionInfo 
						 in  ((sqrt $ fromIntegral ((p1x-cx)^2 + (p1y-cy)^2)) * t2 / (sqrt $ fromIntegral ((p1x-p2x)^2 + (p1y-p2y)^2) ))
    pointFinal= if collisionInfo == Nothing then if nextLevelInfo == Nothing then (0, 0, 0) -- doesn't really matter
									     else let Just (tNext, collNext) = nextLevelInfo in collNext
					    else let Just (collPt@(cx, cy, cz)) = collisionInfo in collPt
    final = if collisionInfo == Nothing && nextLevelInfo == Nothing then Nothing else Just (timeFinal, pointFinal)
								    

-------------------------------------------------------------------------------------
--		Collision detection between player and non-player moving entities
-------------------------------------------------------------------------------------

--two-moving-body collision detection is a problem, not just for line-line, but for rectangle-rectangle collision detection
--the rectangle is formed by the two extreme points on the moving body, perpendicular to the direction of motion
--i.e. I have to grow the playerTrajec by playerRadius perpendicular to player motion direction, 
--and the entityTrajec by entityRadius perpendicular to entity motion direction
--then I have to check if these two RECTANGLES intersect, and if so, 
--make sure that the player and the entity arrive at the intersection point at the same time
--
--rectangle intersection can be done by using the separating axis theorem (SAT)
--If I find an intersection, I will take the mid-point of the intersecting length, 
--and treat that as the collision point
--this is a bit inaccurate, as the ball and the monster do not travel at the same velocities, 
--but it's good enough for simulation

--decided to take simpler case as below
--we have 2 main cases:
--1. parallel trajectories collision detection
--2. non-parallel trajectories collision detection
--

determineEarliestCollisionParallel :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> TimePoints -> Maybe (XoTime, XoPoint3D)
determineEarliestCollisionParallel g frameTime collisionTolerance plMove@(pl1@(pl1x, pl1y, pl1z), pl2@(pl2x, pl2y, pl2z)) ((tp1@(t1, p1@(p1x, p1y, p1z), _)):(tp2@(t2, p2@(p2x, p2y, p2z), _)):tps)
  | (pl1x == pl2x && pl1y == pl2y) = Nothing 	--player didn't move
  | (p1x == p2x && p1y == p2y) = Nothing 	--entity didn't move
  | (pl1x == pl2x && (abs (p1x-p2x) <= collDelta) ) = if (collidexcase) then Just (timexcase, midpxcase) else Nothing
  | (pl1y == pl2y && (abs (p1y-p2y) <= collDelta) ) = if (collideycase) then Just (timeycase, midpycase) else Nothing
  | otherwise = error "trying to find parallel collision in non-parallel trajectories"
    where
      collideycase = if ((abs (pl1y - p1y) < collisionTolerance) && (((pl1x `elem` (take 2 sortedx)) && (pl2x `elem` (drop 2 sortedx))) 
							   	    || ((pl2x `elem` (take 2 sortedx)) && (pl1x `elem` (drop 2 sortedx)))) ) then True else False 
      collidexcase = if ((abs (pl1x - p1x) < collisionTolerance) && (((pl1y `elem` (take 2 sortedy)) && (pl2y `elem` (drop 2 sortedy))) 
							 	    || ((pl2y `elem` (take 2 sortedy)) && (pl1y `elem` (drop 2 sortedy)))) ) then True else False 
      sortedx = sort [pl1x, pl2x, p1x, p2x]
      sortedy = sort [pl1y, pl2y, p1y, p2y]
      dycase = ((sortedx !! 2) - (sortedx !! 1)) `div` 2
      dxcase = ((sortedy !! 2) - (sortedy !! 1)) `div` 2
      midpycase = ((sortedx !! 1) + dycase, pl1y, pl1z)
      midpxcase = (pl1x, (sortedy !! 1) + dxcase, pl1z)
      timeycase = frameTime * (fromIntegral ((sortedx !! 1) + dycase - pl1x)) / fromIntegral (pl2x-pl1x)
      timexcase = frameTime * (fromIntegral ((sortedy !! 1) + dxcase - pl1y)) / fromIntegral (pl2y-pl1y)


--determines the earliest collision between player and non-player moving entity (line-line)
--also makes sure, they both arrive there at the same time (with tolerance collisionTolerance)
--returns the time and the point corresponding to this collision
--time returned is from start of the frameTime
--
determineEarliestCollisionIntersecting :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> TimePoints -> Maybe (XoTime, XoPoint3D)
determineEarliestCollisionIntersecting g frameTime collisionTolerance plMove@(pl1@(pl1x, pl1y, pl1z), pl2@(pl2x, pl2y, pl2z)) timePoints
  = if (null timePoints || (length timePoints) == 1) then Nothing else determineEarliestCollisionIntersecting' g frameTime collisionTolerance plMove timePoints
    
determineEarliestCollisionIntersecting' :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> TimePoints -> Maybe (XoTime, XoPoint3D)
determineEarliestCollisionIntersecting' g frameTime collisionTolerance plMove@(pl1@(pl1x, pl1y, pl1z), pl2@(pl2x, pl2y, pl2z)) ((tp1@(t1, p1@(p1x, p1y, p1z), _)):(tp2@(t2, p2@(p2x, p2y, p2z), _)):tps)
 | (pl1x == pl2x && (abs (p1x-p2x) <= collDelta)) || (pl1y == pl2y && (abs (p1y-p2y) <= collDelta)) 
	  = determineEarliestCollisionParallel g frameTime collisionTolerance plMove (tp1:tp2:tps)
 | otherwise = myCollision
    where
      playerTrajec = XoLine pl1 pl2
      entityTrajec = XoLine p1 p2

      --check if this part of the trajectory collides with player trajectory
      collPt = intersectPoint playerTrajec entityTrajec

      --find amount of distance moved by the player in t2 units of time
      --this is needed so we can displace player by appropriate amount for future collision detection
      dx = if (pl1x == pl2x) then 0 else truncate (fromIntegral(pl2y-pl1y) * t2 / frameTime )
      dy = if (pl1y == pl2y) then 0 else truncate (fromIntegral(pl2x-pl1x) * t2 / frameTime )
      pInt@(pIntx, pInty, pIntz) = (pl1x + dx, pl1y + dy, pl1z)

      --find any future collision point: Note lazy-evaluation ensures us that this is done only if needed :)
      --possible future collision points are only needed if this trajectory did not generate a collision point
      --also, if t2 is more than frameTime, then there is no need to look at next TimePoint
      futureColl = if (t2 > frameTime) then Nothing else determineEarliestCollisionIntersecting g (frameTime - t2) collisionTolerance (pInt, pl2) (tp2:tps)

      --if there is a collision point, make sure that player and entity arrive there at the same times
      timeCollPlayer = if (collPt /= Nothing) 
			  then let Just (point) = collPt 
			       in frameTime * (fromIntegral $ lenTrajec (XoLine pl1 point)) / (fromIntegral $ lenTrajec playerTrajec)
	  		  else 0.0 

      timeCollEntity = if (collPt /= Nothing)
			  then let Just (point) = collPt
			       in t2*(fromIntegral $ lenTrajec (XoLine p1 point)) / (fromIntegral $ lenTrajec entityTrajec)
			  else 0.0

      -- find the distance travelled by the player in timeCollEntity
      -- At this time, entity is at coll point, so if player is within collisionTolerance of the entity, it is a kill
      travelInEntityTimeForPlayer@(tx, ty, tz) = if (pl1x == pl2x) then (pl1x, pl1y + truncate (timeCollEntity*(fromIntegral(pl2y-pl1y))/frameTime), pl1z) 
						     	           else (pl1x + truncate (timeCollEntity*(fromIntegral(pl2x-pl1x))/frameTime), pl1y, pl1z)

      (collpx, collpy, collpz) = if collPt /= Nothing then let Just(colx, coly, colz) = collPt in (colx, coly, colz) else (0, 0, 0) --doesn't really matter

      collHappened = collPt /= Nothing && ((truncate $ sqrt $ fromIntegral $ (tx-collpx)^2 + (ty-collpy)^2 +(tz-collpz)^2) <= collisionTolerance)

      --get final collision point
      collPtFinal = if (not collHappened) then if futureColl == Nothing 
						  then Nothing else let Just (futureCollTime, futureCollPt) = futureColl in Just futureCollPt
				          else collPt

      --get final time for collision
      collTimeFinal= if (not collHappened) then if futureColl == Nothing 
						   then Nothing else let Just (futureCollTime, futureCollPt) = futureColl in Just (t2+futureCollTime)
				           else Just timeCollEntity
      
      myCollision = if (collPtFinal == Nothing) then Nothing else let Just (myPoint) = collPtFinal
								      Just (myTime) = collTimeFinal
								  in  Just (myTime, myPoint)
--also note that sometimes, their trajectories dont actually collide, 
--but the end points are pretty darn close
--this case is also detected, and flagged as a collision at t2 time
determineEarliestDynamicCollision :: XoGame -> XoTime -> XoSize -> (XoPoint3D, XoPoint3D) -> TimePoints -> Maybe (XoTime, XoPoint3D)
determineEarliestDynamicCollision g frameTime collisionTolerance plMove@(pl1@(pl1x, pl1y, pl1z), pl2@(pl2x, pl2y, pl2z)) timePoints  
  | (null timePoints || (length timePoints) == 1) =  Nothing
  | otherwise = let retVal = determineEarliestCollisionIntersecting g frameTime collisionTolerance plMove timePoints 
  		in  if (retVal /= Nothing) then retVal else endTimePoint
    where
      ((tp1@(t1, p1@(p1x, p1y, p1z), d1@(d1x, d1y, d1z))):(tp2@(t2, p2@(p2x, p2y, p2z), _)):tps) = timePoints

      -- we compare the end of the entity @ t2 w/ the player at that time
      -- the end of entity at that time is simply p2
      -- we find the new player position in time t2 now
      -- [note that by design, t2 is always <= frameTime, so this is a legal compare for player and entity]
      deltax = if (pl1y == pl2y) then (ceiling $ fromIntegral (pl2x-pl1x) * t2 / frameTime) else 0
      deltay = if (pl1x == pl2x) then (ceiling $ fromIntegral (pl2y-pl1y) * t2 / frameTime) else 0
      plCompare@(plcx, plcy, plcz) = (pl1x + deltax, pl1y + deltay, pl1z)

      distanceBetweenEnds = truncate $ sqrt $ fromIntegral ((plcx - p2x)^2 + (plcy - p2y)^2 + (plcz - p2z)^2)
      endTimePoint = if (distanceBetweenEnds <= collisionTolerance) then Just (t2, plCompare)
								    else Nothing
      
      
      

