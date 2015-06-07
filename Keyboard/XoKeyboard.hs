-- -------------------------------------------------------------------------------------
--         Author: Sourabh S Joshi (cbrghostrider); Copyright - All rights reserved.
--                       For email, run on linux (perl v5.8.5):
--   perl -e 'print pack "H*","736f75726162682e732e6a6f73686940676d61696c2e636f6d0a"'
-- -------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoKeyboard (processKey) where

import XoData
import XoPowerUp

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

processKey :: XoGame -> Key -> KeyState -> Modifiers -> Position -> XoGame
processKey g (SpecialKey k) Down _ _ = 
  if (dx == (-1 * odx) && dy == (-1 * ody) && (not $ null $ points $ shape poly)) 
	then g 
	else g {player = plNew, polyLine = polyNew}
    where
      pl = player g
      oldDir@(odx, ody, odz) = direction pl
      poly = polyLine g
      dir@(dx, dy, dz) 
	  | k == KeyUp 		= (0, 1, 0)
	  | k == KeyDown 	= (0, -1, 0)
	  | k == KeyLeft	= (-1, 0, 0)
	  | k == KeyRight 	= (1, 0, 0)
	  | otherwise		= direction pl
      --update the player to the new direction
      plNew = pl {direction = dir}
      -- also force update of polyline if needed
      -- FIXME: should really make this a call to method within XoPolyLine
      ptsNew = (points $ shape poly) ++ [(center $ shape pl)]
      shpNew = (shape poly){points = ptsNew}
      polyNew = if (((points $ shape poly) == []) || (dir == (direction pl))) 
 			then poly else poly {shape = shpNew}
      -- FIXME: Note, not sure if above is entirely accurate, since we may have walked into a BB and then changed direction
      -- in which case, the BB intersection needs to be processed first
      -- FIXME: also, if direction is being changed ON the BB edge, then need to figure out what to do?

processKey g (Char ' ') Down _ _ = consumePowerUp g

-- ~ is test key that will force player to be reborn at (30, 30, 0) and to be at rest
processKey g (Char '~') Down _ _ = g {player = (player g){shape = shpNew, direction = (0, 0, 0)}, polyLine = ((polyLine g){shape = ((shape $ polyLine g){points = []})})}
  where
    shpNew = (shape (player g)) {center = (30, 30, 0)}

-- ` is test key that will immediately stop the player, but let the rest of the game go on
processKey g (Char '`') Down _ _ = g {player = (player g){direction = (0, 0, 0)}}

-- r is test key that will reload the simulation to the initGame state
processKey g (Char 'r') Down _ _ = initGame

-- cheat code for getting extra lives
processKey g (Char '+') Down _ _ = g {scoreBd = ((scoreBd g){lives = (lives $ scoreBd g) + 1})}

processKey g _ _ _ _ = g




