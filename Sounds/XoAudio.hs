--------------------------------------------------------------------------------------
--		Haskell Xonix: Author - Sourabh S Joshi
--------------------------------------------------------------------------------------

module XoAudio where

import XoData
--import Sound

data XoSoundEvent = 
    XoPolyStart 
  | XoPolyStop 
  | XoKilled 
  | XoGrenade 
  | XoTurbo 
  | XoLife 
  deriving (Eq, Show)

xonixSoundEvent :: XoSoundEvent -> IO ()
xonixSoundEvent _ = error "Sounds are unimplemented so far! :("
{-
xonixSoundEvent XoPolyStart = do
  playSound ".\\Sounds\chomp.wav" nullAddr [Loop, Async]

xonixSoundEvent XoPolyStop = do
  playSound ".\\Sounds\chomp.wav" nullAddr [Async]

xonixSoundEvent XoKilled = do
  playSound ".\\Sounds\\death.wav" nullAddr [Sync]

-}
