--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoPolyLine (evaluatePolyEdgeEvent) where 

import XoData
import XoBoundingBox

import Data.List

-- a polyEvent will happen when the player either enters or exits a bounding box
-- the main flow is capped to simulate only one such event, 
-- which considerably simplifies the making of polylines and the 
-- disintegration/breaking-up of bounding boxes

-- the polyLine itself is stored as a list of points, 
-- from the first break 
-- to any subsequent turning points, 
-- to the last break, which is immediately resolved

-- this is called when the player presses a direction key, so that a poly turn point can be added
-- beware that if the player presses this at the exact instant that we are on any polyLine, then we need to call 
-- polyEventEnd and evaluate the whole simulation instead!
--

evaluatePolyEdgeEvent :: XoGame -> XoEntity -> (XoShape, XoPoint3D) -> XoGame
evaluatePolyEdgeEvent g newPlayer intersect 
  | (points.shape.polyLine) g == [] = evaluatePolyEventBegin g newPlayer intersect
  | otherwise = evaluatePolyEventEnd g newPlayer intersect

evaluatePolyEventBegin :: XoGame -> XoEntity -> (XoShape, XoPoint3D) -> XoGame
evaluatePolyEventBegin g newPlayer intersect@(shp@(XoLine p1@(px1, py1, pz1) p2@(px2, py2, pz2)), pc@(pcx, pcy, pcz))
  | (pcx /= px1 && pcy /= py1) = error "Polyline: Collision point isn't on the edge it is supposed to be on!"
  | (px1 == px2 || py1 == py2) = if (not (tooSmallCutEdge shp)) then g {polyLine = newPolyLine}
							        else error "PolyLine: Found edge too small to be cut!"
  | otherwise = error "Polyline: beginning at non-x/y parallel edge!"
    where
      traceLine = XoTraceLine [pc]
      newPolyLine = (polyLine g) {shape = traceLine}

evaluatePolyEventEnd :: XoGame -> XoEntity -> (XoShape, XoPoint3D) -> XoGame
evaluatePolyEventEnd g newPlayer intersect@(shp@(XoLine p1@(px1, py1, pz1) p2@(px2, py2, pz2)), pc@(pcx, pcy, pcz))
  | (points.shape.polyLine) g == [] = error "Polyline: Cannot end polyLine that does not exist!"
  | (px1 == px2 && px1 == sx) || (py1 == py2 && py1 == sy) = evaluatePolyEndSameEdgesCut updatedG newPlayer intersect 
  | otherwise = evaluatePolyEndDiffEdgesCut updatedG newPlayer intersect 
    where
      polyLineRecord = polyLine g
      startLinePoint@(sx, sy, sz) = (head.points.shape) polyLineRecord
      updatedPolyPoints = (points $ shape $ polyLineRecord) ++ [pc]
      updatedPolyShape = (shape polyLineRecord){points = updatedPolyPoints}
      updatedPolyRecord = polyLineRecord{shape = updatedPolyShape}
      updatedG = g{polyLine = updatedPolyRecord}

evaluatePolyEndSameEdgesCut :: XoGame -> XoEntity -> (XoShape, XoPoint3D) -> XoGame
evaluatePolyEndSameEdgesCut g newPlayer intersect@(shp@(XoLine p1@(px1, py1, pz1) p2@(px2, py2, pz2)), pc@(pcx, pcy, pcz)) = newGame
  where
    affectedPolygon = findAffectedPolygon g pc
    polyTraceOriginal = points $ shape $ polyLine g 
    --find start and end points of original poly trace
    startPolyTrace@(spx, spy, spz) 	= head $ polyTraceOriginal
    endPolyTrace@(epx, epy, epz) 	= last $ polyTraceOriginal
    --find the edge where BB will be split into two BBs
    affectedEdge@(tl@(tx, ty, tz), hd@(hx, hy, hz)) = getAffectedEdgeFromPolygon affectedPolygon pc ((last.init) polyTraceOriginal)
    isPolyTraceCCW  
      | tx == hx = (ty < hy && spy > epy) || (ty > hy && spy < epy)
      | ty == hy = (tx < hx && spx > epx) || (tx > hx && spx < epx)
      | otherwise = error "PolyLine: Oopsie! messed up affected edge"
    polyTraceCCWOriented = if (isPolyTraceCCW) then polyTraceOriginal else reverse polyTraceOriginal
    -- find the inner and outer poly traces (polyWidth) apart and CCW oriented
    (leftPoly, rightPoly) = findCCWLeftRightPolyLines polyTraceCCWOriented affectedEdge affectedEdge
    -- FIXME, if start or end points of the leftPoly or the rightPoly are too close (<polyWidth) to the 
    -- end point of the affected Edge, then make the end point of the poly the end pt of affected edge
    -- and also carry this delta manipulation in x or y over to one point before end pt of poly and 
    -- one point after start point of poly
    -- note that in same edge cut case, you only need to do this for right poly
    -- but in different edge cut case, both new poly's qualify for this potential change
    -- end FIXME
    -- now splice these polyLine traces to get 2 BB from 1 (they may be empty which will be checked later)
    innerPolygon = affectedPolygon {shape = XoPolygon leftPoly}
    (prevPoints, (tl':laterPoints)) = break (\x -> x == tl) $ points $ shape $ affectedPolygon
    mainPolygon = affectedPolygon {shape = XoPolygon (prevPoints ++ (tl:rightPoly) ++ laterPoints) }
    -- then remove the original affected polygon, and replace it by these two
    newBBList = innerPolygon : mainPolygon : (delete affectedPolygon $ boundingBoxes g)
    --and remove this polyline which has been consumed
    newPolyShape = (shape $ polyLine g){points = []}
    newPolyLine = (polyLine g){shape = newPolyShape}
    newGame = g {boundingBoxes = newBBList, polyLine = newPolyLine}    
      
evaluatePolyEndDiffEdgesCut :: XoGame -> XoEntity -> (XoShape, XoPoint3D) -> XoGame
evaluatePolyEndDiffEdgesCut g newPlayer intersect@(shp@(XoLine p1@(px1, py1, pz1) p2@(px2, py2, pz2)), pc@(pcx, pcy, pcz)) = newGame
  where
    affectedPolygon = findAffectedPolygon g pc
    polyTraceOriginal = points $ shape $ polyLine g
    startPolyTrace@(spx, spy, spz) 	= head $ polyTraceOriginal
    endPolyTrace@(epx, epy, epz) 	= last $ polyTraceOriginal
    --find the affected edges at entry and exit
    affEdgeEntry@(tlAffEntry@(tlAffEntx, tlAffEnty, tlAffEntz), hdAffEntry@(hdAffEntx, hdAffEnty, hdAffEntz))
      = getAffectedEdgeFromPolygon affectedPolygon startPolyTrace ((head.tail) polyTraceOriginal)
    affEdgeExit@(tlAffExit@(tlAffExx, tlAffExy, tlAffExz), hdAffExit@(hdAffExx, hdAffExy, hdAffExz))
      = getAffectedEdgeFromPolygon affectedPolygon endPolyTrace ((last.init) polyTraceOriginal)
    (leftPoly, rightPoly) = findCCWLeftRightPolyLines polyTraceOriginal affEdgeEntry affEdgeExit
    --FIXME: Keep in mind same FIXME as note above for SameEdgesCut version
    --end FIXME
    --now rearrange the original polygon points list, so that entry edge's head is listed first, 
    --followed in CCW order by other points (including somewhere the exit edge's tail)
    --and ending obviously with the entry edge's tail 
    --i.e. [entry edge head, ............., exit edge tail, exit edge head, ..........., entry edge tail]
    (ptsToMove, ptsToStart) = break (==hdAffEntry) (points $ shape affectedPolygon)
    rearrangedPolygon = ptsToStart ++ ptsToMove
    -- now get the points that go in the new polygon1 and polygon2 in CCW order also
    -- this is obtained by breaking on *head* of exit edge (draw pictures if you want to figure out why)
    (polygon1Pts, polygon2Pts) = break (==hdAffExit) rearrangedPolygon
    (polygon1AllPts, polygon2AllPts) = (polygon1Pts ++ rightPoly, polygon2Pts ++ leftPoly)
    polygon1 = affectedPolygon {shape = XoPolygon polygon1AllPts}
    polygon2 = affectedPolygon {shape = XoPolygon polygon2AllPts}
    -- then remove the original affected polygon, and replace it by these two
    newBBList = polygon1 : polygon2 : (delete affectedPolygon $ boundingBoxes g)
    --and remove this polyline which has been consumed
    newPolyShape = (shape $ polyLine g){points = []}
    newPolyLine = (polyLine g){shape = newPolyShape}
    newGame = g {boundingBoxes = newBBList, polyLine = newPolyLine}    


-- identifies the bounding box the given point lies on, by checking if it lies on any of the edges of the BB
findAffectedPolygon :: XoGame -> XoPoint3D -> XoEntity
findAffectedPolygon g intPt = entityFound
  where
    entityFound = if (null candidates) then error "PolyLine:Intersect point does not lie on any polygon!"
				       else if ((length candidates) > 1) 
					        then error "PolyLine: Mutiple polygons contain intersect point!"
						else (fst $ head candidates)
    candidates = filter (\(bb, tr) -> tr == True) $ zip (boundingBoxes g) $ map (any (\x->x == True)) 
			$ map (map (isPointOnEdge intPt)) $ map (getListOfBoundingBoxEdges) $ boundingBoxes g 

isPointOnEdge :: XoPoint3D -> (XoPoint3D, XoPoint3D) -> Bool
isPointOnEdge ip@(ipx, ipy, ipz) ed@(p1@(p1x, p1y, p1z), p2@(p2x, p2y, p2z)) 
  | p1x == p2x && p1x == ipx && (((sort [ipy, p1y, p2y]) !! 1) == ipy) = True
  | p1y == p2y && p1y == ipy && (((sort [ipx, p1x, p2x]) !! 1) == ipx) = True
  | otherwise = False

--find the edge of the polygon on which the poly start or end point lies
--checkpoint is the other point on the last segment of the poly (other than point of collision pc)
--we need checkpoint, in the case where intersection is at an edge (i.e. 2 edges)... 
--we will pick the perpendicular edge
getAffectedEdgeFromPolygon :: XoEntity -> XoPoint3D -> XoPoint3D -> (XoPoint3D, XoPoint3D)
getAffectedEdgeFromPolygon checkPolygon pc checkPoint = affectedEdge
  where
    affectedEdgeCandidates = filter (\(_, x)-> (x == True)) $ zip (getListOfBoundingBoxEdges checkPolygon) 
				$ map (isPointOnEdge pc) $ getListOfBoundingBoxEdges checkPolygon
    affectedEdge   
      | (null affectedEdgeCandidates) = error "PolyLine: Oops! Too few affected edges." 
      | (length affectedEdgeCandidates == 1) = fst $ head affectedEdgeCandidates
      | (length affectedEdgeCandidates) == 2 = resolveTwoAffectedEdges pc checkPoint affectedEdgeCandidates 
      | otherwise = error "PolyLine: Oops! Too many (>2) affected edges." 

--pc is point of collision on edge
--cp is check point i.e. other point on the last segment of the poly
resolveTwoAffectedEdges :: XoPoint3D -> XoPoint3D -> [((XoPoint3D, XoPoint3D), Bool)] -> (XoPoint3D, XoPoint3D)
resolveTwoAffectedEdges pc@(pcx, pcy, pcz) cp@(cpx, cpy, cpz) ( ((e1p1@(e1p1x, e1p1y, e1p1z), e1p2@(e1p2x, e1p2y, e1p2z)), _) : ((e2p1@(e2p1x, e2p1y, e2p1z), e2p2@(e2p2x, e2p2y, e2p2z)), _) :[]) 
  | (e1p1x == e1p2x) && (pcy == cpy) = (e1p1, e1p2) -- case where edge is parallel to y and poly is parallel to x 
  | (e1p1y == e1p2y) && (pcx == cpx) = (e1p1, e1p2) -- case where edge is parallel to x and poly is parallel to y
  | (e2p1x == e2p2x) && (pcy == cpy) = (e2p1, e2p2) -- case where edge is parallel to y and poly is parallel to x 
  | (e2p1y == e2p2y) && (pcx == cpx) = (e2p1, e2p2) -- case where edge is parallel to x and poly is parallel to y
  | otherwise = error "PolyLine: Cannot resolve 2 edge candidate case on poly completion!"

resolveTwoAffectedEdges _ _ _ = error "PolyLine: 2 edges expected!"

------------------------------------------------------------------------------------------
--				    Utility functions
------------------------------------------------------------------------------------------

-- note that too small cut edges shouldn't arise, because we should be able to manipulate the 
-- cut parts of a polygon (when cut previously by a polyLine) so that no super-thin edges remain
tooSmallCutEdge :: XoShape -> Bool
tooSmallCutEdge shp@(XoLine p1@(px1, py1, pz1) p2@(px2, py2, pz2))
  | (px1 == px2 && abs(py1 - py2) < polyLineWidth) = True -- super thin edge case
  | (py1 == py2 && abs(px1 - px2) < polyLineWidth) = True -- super thin edge case
  | (px1 /= px2 && py1 /= py2) = error "Cut edge is non x/y parallel"
  | otherwise = False

tooSmallCuttingEdge _ = error "Found non-line shaped cutting edge"


-- this method, given a CCW polyline, splits it into two polylines, 
-- one is to the left, and other to the right of the original polyline
-- the one to the left has its points arranged in CCW direction, 
-- and the one to the right has its points arranged in a CCW direction also 
-- this is so that we can easily splice these polylines to the appropriate polygons
findCCWLeftRightPolyLines :: [XoPoint3D] -> (XoPoint3D, XoPoint3D) -> (XoPoint3D, XoPoint3D) -> ([XoPoint3D], [XoPoint3D])
findCCWLeftRightPolyLines [] _ _ = error "PolyLine: Empty Polyline error!"
findCCWLeftRightPolyLines (p:[]) _ _ = error "PolyLine: Not enough poly line points error!"
findCCWLeftRightPolyLines pseq@(p1@(p1x, p1y, p1z):p2@(p2x, p2y, p2z):ps) 
  entry@(enttail@(enttx, entty, enttz), entryhead@(enthx, enthy, enthz))
  eexit@(extail@(extx, exty, extz), exhead@(exhx, exhy, exhz))
     = (leftPoints, rightPoints)
       where
         headPoly@(hx, hy, hz) = head pseq
         lastPoly@(lx, ly, lz) = last pseq
         displacementPointsRaw = findCCWPolyLineDisplacementPoints pseq
         displacementPointsleftRight@(ls, rs) = (map fst displacementPointsRaw, map snd displacementPointsRaw)
         (startLeft, startRight)
           | enttx == enthx = if entty < enthy 	then ((hx, hy - t, hz), (hx, hy + t, hz))
			           		else ((hx, hy + t, hz), (hx, hy - t, hz))
           | entty == enthy = if enttx < enthx 	then ((hx - t, hy, hz), (hx + t, hy, hz))
	 		           		else ((hx + t, hy, hz), (hx - t, hy, hz))
           | otherwise = error "PolyLine: non x/y parallel coordinates!"
         (endLeft, endRight)
           | extx == exhx = if exty < exhy 	then ((lx, ly + t, lz), (lx, ly - t, lz))
			           		else ((lx, ly - t, lz), (lx, ly + t, lz))
           | exty == exhy = if extx < exhx 	then ((lx + t, ly, lz), (lx - t, ly, lz))
			           		else ((lx - t, ly, lz), (lx + t, ly, lz))
           | otherwise = error "PolyLine: non x/y parallel coordinates!"
         (leftPoints, rightPoints) = (startLeft:ls ++ [endLeft], reverse (startRight:rs ++ [endRight]))
         t = polyLineWidth `div` 2

--this method takes the polyLine points (in CCW direction) as input
--it disregards the very first and the very last point in the input
--it gives us a pair for every other polyline point
--this pair is a pair of the point to the (left, right) of the original polyLine point, 
--if we walked the poly line in a CCW direction from start to end
--the left ones will be CCW when read in order
--the right ones will be CW when read in order
findCCWPolyLineDisplacementPoints :: [(XoPoint3D)] -> [(XoPoint3D, XoPoint3D)]
findCCWPolyLineDisplacementPoints (p1@(p1x, p1y, p1z):p2@(p2x, p2y, p2z):p3@(p3x, p3y, p3z):ps) = leftRightPair : findCCWPolyLineDisplacementPoints (p2:p3:ps)
  where 
    leftRightPair
      | horRightLeftTurn 	= ((p2x - t , p2y + t, p2z), (p2x + t, p2y - t, p2z))
      | horRightRightTurn 	= ((p2x + t , p2y + t, p2z), (p2x - t, p2y - t, p2z)) 
      | horLeftRightTurn	= ((p2x - t , p2y - t, p2z), (p2x + t, p2y + t, p2z)) 
      | horLeftLeftTurn		= ((p2x + t , p2y - t, p2z), (p2x - t, p2y + t, p2z)) 
      | verUpRightTurn		= ((p2x - t , p2y + t, p2z), (p2x + t, p2y - t, p2z)) 
      | verUpLeftTurn		= ((p2x - t , p2y - t, p2z), (p2x + t, p2y + t, p2z)) 
      | verBottomLeftTurn	= ((p2x + t , p2y + t, p2z), (p2x - t, p2y - t, p2z)) 
      | verBottomRightTurn	= ((p2x + t , p2y - t, p2z), (p2x - t, p2y + t, p2z)) 
      | otherwise 		= error "PolyLine: Unknown turn orientation!"
    horRightLeftTurn 		= (p1y == p2y) && (p1x < p2x) && (p2y < p3y)
    horRightRightTurn 		= (p1y == p2y) && (p1x < p2x) && (p2y > p3y)
    horLeftRightTurn		= (p1y == p2y) && (p1x > p2x) && (p2y < p3y)
    horLeftLeftTurn		= (p1y == p2y) && (p1x > p2x) && (p2y > p3y)
    verUpRightTurn		= (p1x == p2x) && (p1y < p2y) && (p2x < p3x)
    verUpLeftTurn		= (p1x == p2x) && (p1y < p2y) && (p2x > p3x)
    verBottomLeftTurn		= (p1x == p2x) && (p1y > p2y) && (p2x < p3x)
    verBottomRightTurn		= (p1x == p2x) && (p1y > p2y) && (p2x > p3x)
    t = polyLineWidth `div` 2
    
findCCWPolyLineDisplacementPoints _ = []


-- TODO: What to do in the following scenarios?
--     1. Polyline trace is such that player is going parallel and along a BB edge?
--       *We might want to perturb the player such that it is just outside or inside the 
--        polygon (preferably outside).
--     2. A key is pressed, and it turns out that between the last player position, 
--         and the key press, we had crossed into a BB. Do we have to do the simulation
--         by limiting the frameTime to entryTime, but still accomodate the turn later?
--        *I guess we can start polyLine, then simulate and get new game state
--         then start polyLine and turn it as desired, then simulate the new game state
--         and just display this last one. What if the first game state ends the game?
--     3. polyline starts cutting through a very thin and long polygon, parallel to the long edge but in 
--        the middle of the polygon thickness (very thin), and the outsides of the polyline fall
--        outside the polygon itself!? 
--       *we should really be able to erase the polygon as the player moves!
--     4. what if we are about to exit a polygon but turn at the last moment?
--     	 *record the last turn point as just enough away from the edge

