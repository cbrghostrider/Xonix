--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoBoundingBox (getCollideBB, getListOfBoundingBoxEdges, growBoundingBoxEdges,
		      isPointInsideBoundingBox, isPointOutsideBoundingBox,
 		      isEmptyBoundingBox, computeAreaOfBB) where
import XoData
import XoCollision
import Graphics.Rendering.OpenGL

--------------------------------------------------------------------------------------
--			Bounding box algorithms
--------------------------------------------------------------------------------------

getCollideBB :: XoShape -> [(XoPoint3D, XoPoint3D)] -> [((XoPoint3D, XoPoint3D), Maybe XoPoint3D)]
getCollideBB trajec edges = zip edges $ map (intersectPoint trajec) $ map (\(p1, p2) -> XoLine p1 p2) $ edges

-- to get list of all edges in a bounding box, along with a tag on edge number
getListOfBoundingBoxEdges :: XoEntity -> [(XoPoint3D, XoPoint3D)]
getListOfBoundingBoxEdges bbEntity = bbEdges
  where
    bbPts = getPoints $ shape bbEntity
    getPoints (XoPolygon pol) = pol
    getPoints _ = error "Could not get points from non-polygon shape in getListOfBoundingBoxEdges"
    bbEdges = (zip (init bbPts) (tail bbPts)) ++ [(last bbPts, head bbPts)]

shrinkBoundingBoxEdges 	:: XoSize ->  [(XoPoint3D, XoPoint3D)] -> [(XoPoint3D, XoPoint3D)]
shrinkBoundingBoxEdges 	= boundingBoxManip False

growBoundingBoxEdges 	:: XoSize ->  [(XoPoint3D, XoPoint3D)] -> [(XoPoint3D, XoPoint3D)]
growBoundingBoxEdges 	= boundingBoxManip True

--BB growth algorithms assume that points are in counter-clockwise direction
boundingBoxManip :: Bool -> XoSize ->  [(XoPoint3D, XoPoint3D)] -> [(XoPoint3D, XoPoint3D)]
boundingBoxManip grow rad bbPts = map (bbEdgeManip grow rad) bbPts

bbEdgeManip :: Bool -> XoSize -> (XoPoint3D, XoPoint3D) -> (XoPoint3D, XoPoint3D)
bbEdgeManip grow rad (p1@(p1x, p1y, _), p2@(p2x, p2y, _)) 
  | p1x == p2x 
    = if (p1y < p2y) 
	 then ((p1x + rad*factor, p1y - rad*factor, 0), (p2x + rad*factor, p2y + rad*factor, 0))
	 else ((p1x - rad*factor, p1y + rad*factor, 0), (p2x - rad*factor, p2y - rad*factor, 0))
  | p1y == p2y 
    = if (p1x < p2x) 
	 then ((p1x - rad*factor, p1y - rad*factor, 0), (p2x + rad*factor, p2y - rad*factor, 0))
	 else ((p1x + rad*factor, p1y + rad*factor, 0), (p2x - rad*factor, p2y + rad*factor, 0))
  | otherwise = error "Found non x/y parallel edge in bbEdgeManip!"
    where		
      factor = if (grow == True) then 1 else -1


--standing at the tail of the vector, and looking towards the head, 
--is the point to the left of the vector
isPointToVectorLeft :: XoPoint3D -> (XoPoint3D, XoPoint3D) -> Bool
isPointToVectorLeft p@(px, py, pz) vec@(pt@(ptx, pty, ptz), ph@(phx, phy, phz)) 
  | ptx == phx = if (pty < phy) then px < ptx else px > ptx
  | pty == phy = if (ptx < phx) then py > pty else py < pty
  | otherwise = error "non x/y parallel vector passed for point-vector test!"

isPointToVectorRight :: XoPoint3D -> (XoPoint3D, XoPoint3D) -> Bool
isPointToVectorRight pt vec= not $ isPointToVectorLeft pt vec

-- finds out if the bounding box has any balls in it or not
-- CAUTION: For correct simulation, remember that this function needs 
-- the updated ball positions, and not the old ones
--
isEmptyBoundingBox :: XoGame -> XoEntity -> Bool
isEmptyBoundingBox gNew bb = all ( == False) $ map (isPointInsideBoundingBox bb) $ map (center.shape) $ balls gNew

--using the even-odd rule or raycasting algorithm, to decide if a point is inside the bounding box or outside it
--
--ideally only shooting one ray should be enough, but if we intersect at a vertex, 
--it should be counted only once, but will actually be counted twice
--
--to reduce the probability of this messing up my results, I will shoot 4 rays, and take max of those results
--the rays will be going parallel to +x/-x/+y/-y starting from the point
isPointInsideBoundingBox :: XoEntity -> XoPoint3D -> Bool
isPointInsideBoundingBox bb testPt@(tpx, tpy, _) = if (numOdd > numEven) then True else False
  where
    bbEdges = getListOfBoundingBoxEdges bb
    testPoints = [(tpx, _GAME_WINDOW_WIDTH*2, 0), (tpx, (-_GAME_WINDOW_WIDTH*2), 0), (_GAME_WINDOW_WIDTH*2, tpy, 0), ((-_GAME_WINDOW_WIDTH*2), tpy, 0)]
    testLines = map (XoLine testPt) $ testPoints
    intersectionsPerTestLine = map (\x -> findNumIntPerTestLine x bbEdges) testLines
    findNumIntPerTestLine testLine edges = length $ filter (/= Nothing) $ map (\(p1, p2) -> intersectPoint testLine (XoLine p1 p2)) edges 
    numOdd  = length $ filter odd intersectionsPerTestLine
    numEven = length $ filter even intersectionsPerTestLine

isPointOutsideBoundingBox :: XoEntity -> XoPoint3D -> Bool
isPointOutsideBoundingBox bb testpt = not $ isPointInsideBoundingBox bb testpt


computeAreaOfBB :: XoShape -> GLsizei
computeAreaOfBB (shp@(XoPolygon ps)) = sum $ map (\(x, y) -> areaTrapezoidal x y) $ pairsOfPts 
  where 
    -- get CW list of points
    ps' = reverse ps
    pairsOfPts = zip ps' $ (tail ps') ++ [(head ps')]

computeAreaOfBB _ = error "Cannot compute area of non-polygon bounding box!"

-- when p1 and p2 are CW on the polygon, with p1 before p2
areaTrapezoidal :: XoPoint3D -> XoPoint3D -> GLsizei   
areaTrapezoidal p1@(p1x, p1y, p1z) p2@(p2x, p2y, p2z) 
  = sign * (base * smallHt + truncate (0.5 * fromIntegral diffHt * fromIntegral base)) 
    where
      base = abs (p1x-p2x)
      smallHt = if p1y< p2y then p1y else p2y
      diffHt = if p1y < p2y then p2y-p1y else p1y-p2y
      sign = if p1x < p2x then 1 else (-1)
    


