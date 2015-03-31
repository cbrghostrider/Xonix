{- 

HOpenGL Tutorial
http://www.cin.ufpe.br/~hopengl/
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This is a HOpenGL Sound module (supported only in win32 platforms... :[).
Thanks to Monique Monteiro (mlbm@cin.ufpe.br).

-}

module Sound (
	SoundFlag (..), playSound
) where

import Foreign
import CString
import IOExts
import Data.Bits
import Graphics.Win32

data SoundFlag
  = Application
  | Alias
  | AliasId
  | Async
  | Filename
  | Loop
  | Memory
  | NoDefault
  | NoStop
  | NoWait
  | Purge
  | Resource
  | Sync
  deriving ()

marshall_SoundFlag :: SoundFlag -> Word32
marshall_SoundFlag arg1 = 
  case arg1 of {
     Application -> ``SND_APPLICATION'';
     Alias       -> ``SND_ALIAS'';
     AliasId     -> ``SND_ALIAS_ID'';
     Async       -> ``SND_ASYNC'';
     Filename    -> ``SND_FILENAME'';
     Loop        -> ``SND_LOOP'';
     Memory      -> ``SND_MEMORY'';
     NoDefault   -> ``SND_NODEFAULT'';
     NoStop      -> ``SND_NOSTOP'';
     NoWait      -> ``SND_NOWAIT'';
     Purge       -> ``SND_PURGE'';
     Resource    -> ``SND_RESOURCE'';
     Sync        -> ``SND_SYNC''
  }
  
marshall_SoundFlagLIST :: [SoundFlag] -> Word32
marshall_SoundFlagLIST [] = 0
marshall_SoundFlagLIST (d:ds) = marshall_SoundFlag d .|. marshall_SoundFlagLIST ds
  
foreign import ccall "PlaySound"  play :: CString -> Win32.HMODULE -> Word32 -> IO Bool

playSound :: String -> Win32.HMODULE -> [SoundFlag] -> IO Bool
playSound filename hmod flags = play (unsafePerformIO (newCString filename)) hmod (marshall_SoundFlagLIST flags)
