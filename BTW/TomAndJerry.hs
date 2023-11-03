import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- binary tree initialization
layer = 5
create_tree :: Int -> Bin
create_tree 0 = L
create_tree n = B (create_tree (n-1)) (create_tree (n-1))
a_tree = create_tree 4
T_cxt = Hole
T_tree = create_tree layer
J_cxt = Hole
J_tree = create_tree layer


-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to Tom and Jerry.\n"
  putStrLn "You play the role of the hated Tom. Deal with it.\n"
  putStrLn "You start at the root of a (full, balanced, with 5 layers) tree.\n"
  putStrLn "The mouse Jerry is at a leave.\n"
  putStrLn "Your job is to catch it before it reaches the root and escapes.\n"
  putStrLn "    (you can camp at the root forever, but Jerry would not just turn itself in)\n"
  putStrLn "You know, from your numerous failure encouters, that Jerry is quite smart, and you are a bit dull.\n"
  putStrLn "Press ENTER for instruction.\n"
  line <- getLine
  hFlush stdout
  putStrLn "DCM combiii.\n"
  putStrLn "Ignore the previous line. Anyway:\n"
  putStrLn "At each time step, type 'go left', 'go right' or 'go down' to traverse the tree.\n"
  putStrLn "Once you move, Jerry feels the vibration. It then would either stay still or leaf to an adjacent node.\n"
  putStrLn "At any times, if you are at the same node as Jerry, you will catch it and win.\n"
  putStrLn "If after a while it reaches the root and you are not there, it escapes and you lose.\n"
  putStrLn "Be fast. Your time is recorded.\n"
  putStrLn "Press ENTER for more instructions when done.\n"
  line <- getLine
  hFlush stdout
  putStrLn "After a move, you can feel if the mouse moves or not after that.\n"
  putStrLn "You can also feel how far the mouse is in nodes.\n"
  putStrLn "    (you may not be very smart, but you do have cat power.)\n"
  putStrLn "You also have a tracker that shows you the nodes you have been to.\n"
  putStrLn "At any time step, type 'use tracker' to have a peek at it.\n"
  putStrLn "This is a HUGE help, so feel free to ignore it if you feel like it.\n"
  putStrLn "The number of time you use the tracker is recorded.\n"
  putStrLn "Press ENTER to start the game when you are ready.\n"
  putStrLn "Let the hunt begin!.\n"
  line <- getLine
  hFlush stdout
  putStrLn "Game started.\n"

  go (Hole,a_tree)
  where
    go :: BinZip -> BinZip  -> IO ()
    go z = do                                          -- give the player some information
      case z of                                        -- about the current position in the tree
        (_,L)     -> putStrLn "You see a leaf."
        (_,B _ _) -> putStrLn "You see a binary node."
      putStr "> "                                      -- print the prompt
      hFlush stdout                                    -- flush standard output
      line <- getLine                                  -- get a line of input
      case parseInput parseCmd line of                 -- parse the input
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z

          Just Go_Left ->
            case z of
              (c,B t1 t2) -> go (B0 c t2,t1)           -- climb up to the left
              (c,L) -> do
                putStrLn "You cannot climb any further."
                go z

          Just Go_Right ->
            case z of
              (c,B t1 t2) -> go (B1 t1 c,t2)           -- climb up to the right
              (c,L) -> do
                putStrLn "You cannot climb any further."
                go z

          Just Go_Down ->
            case z of
              (B0 c t2,t) -> go (c,B t t2)             -- climb down from the left, or
              (B1 t1 c,t) -> go (c,B t1 t)             -- climb down from the right, or
              (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go z

          Just (Meditate n) -> do
            putStrLn "You close your eyes and focus on your surroundings."
            threadDelay (n * 1000000)
            putStrLn "You open your eyes."
            go z

          Just Quit -> do
            putStrLn "Okay."
            putStrLn "You ended the game over here:\n"
            putStrLn (drawBinZip z)
            putStrLn "Goodbye."
            return ()

main = repl
