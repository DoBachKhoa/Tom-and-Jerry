module Cmd where
import Bin

data Cmd = Go_Left | Go_Right | Go_Down | Stand_Still | Use_Tracker | Quit
  deriving (Show,Read)

move :: BinZip -> Cmd -> Maybe BinZip
move t Go_Left = (go_left t)
move t Go_Right = (go_right t)
move t Go_Down = (go_down t)
move t Stand_Still = Just t