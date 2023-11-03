module Cmd where

data Cmd = Go_Left | Go_Right | Go_Down | Stand_Still | Use_Tracker |Quit
  deriving (Show,Read)
