import Bin
import Cmd
import Parser

import System.IO
import System.Random
import Control.Concurrent (threadDelay)

-- binary tree initialization
layer = 5

data Game = Game {
  mapTom :: BinZip,
  mapJerry :: BinZip,
  nTurn :: Int,
  nTrack :: Int
} deriving (Eq, Show)

init_J_tree :: BinZip -> Int -> BinZip
init_J_tree c 1 = c
init_J_tree c v = if v `mod` 2 == 0
  then init_J_tree lc (v `div` 2)
  else init_J_tree rc (v `div` 2)
    where
      (Just lc) = (go_left c)
      (Just rc) = (go_right c)

checkWin  = (\g -> (distance (fst (mapTom g)) (fst (mapJerry g))) == 0)
checkLose = (\g -> (distance (fst (mapJerry g)) Hole) == 0)
checkEnd :: Game -> IO Bool
checkEnd g = 
  case checkWin g of
    True  -> do
      putStrLn "You caught Jerry the mouse. Well done."
      putStrLn $ "You moved " ++ show (nTurn g) ++ " time(s) and use the tracker" ++ show (nTrack g) ++ " time(s)"
      putStrLn "Your tracker looks like this:\n"
      putStrLn (drawTracker (mapTom g) (mapJerry g))
      return True
    False -> case checkLose g of
        True -> do
          putStrLn "Jerry the mouse reaches the root and excapes. YOU FAILED."
          putStrLn $ "You moved " ++ show (nTurn g) ++ " time(s) and use the tracker" ++ show (nTrack g) ++ " time(s)"
          putStrLn "Your tracker looks like this:\n"
          putStrLn (drawTracker (mapTom g) (mapJerry g))
          return True
        False -> return False

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to Tom and Jerry."
  putStrLn "You play the role of the hated Tom. Deal with it."
  putStrLn "You start at the root of a (full, balanced, with 5 layers) tree."
  putStrLn "The mouse Jerry is at a leave."
  putStrLn "Your job is to catch it before it reaches the root and escapes."
  putStrLn "    (you can camp at the root forever, but Jerry would not just turn itself in)"
  putStrLn "You know, from your numerous failure encouters, that Jerry is quite smart, and you are a bit dull."
  putStrLn "Press ENTER for instruction."
  line <- getLine
  hFlush stdout
  putStrLn "At each time step, type 'go left', 'go right' or 'go down' to traverse the tree."
  putStrLn "Once you move, Jerry feels the vibration. It then would either stay still or leaf to an adjacent node."
  putStrLn "At any times, if you are at the same node as Jerry, you will catch it and win."
  putStrLn "If after a while it reaches the root and you are not there, it escapes and you lose."
  putStrLn "Be fast. Your time is recorded."
  putStrLn "Press ENTER for more instructions when done."
  line <- getLine
  hFlush stdout
  putStrLn "After a move, you can feel if the mouse moves or not after that."
  putStrLn "You can also feel how far the mouse is in nodes."
  putStrLn "    (you may not be very smart, but you do have cat power)"
  putStrLn "You also have a tracker that shows you the nodes you have been to."
  putStrLn "At any time step, type 'use tracker' to have a peek at it."
  putStrLn "This is a HUGE help, so feel free to ignore it if you feel like it."
  putStrLn "The number of time you use the tracker is recorded."
  putStrLn "Press ENTER to start the game when you are ready."
  putStrLn "Let the hunt begin!."
  line <- getLine
  hFlush stdout
  putStrLn "Game started.\n"

  randomNumber <- getStdRandom (randomR (2^(layer-2)+1, 2^layer))

  go Game {
    mapTom = (Hole, B (U (layer - 1)) (U (layer - 1))),
    mapJerry = init_J_tree (Hole, B (U (layer - 1)) (U (layer - 1))) randomNumber,
    nTurn = 0,
    nTrack = 0
  }
  
  where
    go :: Game -> IO ()
    go z = do                                           -- give the player some information
      case (mapTom z) of                                -- about the current position in the tree
        (_, U (-1)) -> putStrLn "You are now in a leaf."
        (_, B _ _)  -> putStrLn "You are now in a binary node."
      
      putStr "> "                                      -- print the prompt
      hFlush stdout                                    -- flush standard output
      line <- getLine                                  -- get a line of input
      case parseInput parseCmd line of                 -- parse the input
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z

          Just Go_Left ->
            case (go_left (mapTom z)) of
              Nothing -> (putStrLn "You cannot climb any further." >> go z)
              Just z' -> do
                putStrLn "You climbed left."
                putStrLn $ "You are " ++ show (distance (fst z') (fst (mapJerry z))) ++ " node(s) away from Jerry."
                temp <- checkEnd z''
                case temp of
                  True  -> return ()
                  False -> goJerry z''
                where z'' = Game {
                                    mapTom = z',
                                    mapJerry = (mapJerry z),
                                    nTurn = (nTurn z)+1,
                                    nTrack = (nTrack z)
                                  }

          Just Go_Right ->
            case (go_right (mapTom z)) of
              Nothing -> (putStrLn "You cannot climb any further." >> go z)
              Just z' -> do
                putStrLn "You climbed right."
                putStrLn $ "You are " ++ show (distance (fst z') (fst (mapJerry z))) ++ " node(s) away from Jerry."
                temp <- checkEnd z''
                case temp of
                  True  -> return ()
                  False -> goJerry z''
                where z'' = Game {
                                    mapTom = z',
                                    mapJerry = (mapJerry z),
                                    nTurn = (nTurn z)+1,
                                    nTrack = (nTrack z)
                                  }

          Just Go_Down ->
            case (go_down (mapTom z)) of
              Nothing -> (putStrLn "You cannot climb any further." >> go z)
              Just z' -> do
                putStrLn "You climbed down."
                putStrLn $ "You are " ++ show (distance (fst z') (fst (mapJerry z))) ++ " node(s) away from Jerry."
                temp <- checkEnd z''
                case temp of
                  True  -> return ()
                  False -> goJerry z''
                where z'' = Game {
                                   mapTom = z',
                                   mapJerry = (mapJerry z),
                                   nTurn = (nTurn z)+1,
                                   nTrack = (nTrack z)
                                 }

          Just Stand_Still -> do
            putStrLn "You did not move."
            goJerry Game {
              mapTom = (mapTom z),
              mapJerry = (mapJerry z),
              nTurn = (nTurn z)+1,
              nTrack = (nTrack z)
            }

          Just Use_Tracker -> do
            putStr "You peeked at the tracker. This is the screen.\n"
            putStrLn (drawTracker (mapTom z) (mapJerry z))
            go Game {
              mapTom = (mapTom z),
              mapJerry = (mapJerry z),
              nTurn = (nTurn z),
              nTrack = (nTrack z) + 1
            }

          Just Quit -> do
            putStrLn "You fell down the tree and died."
            putStrLn "Your tracker looks like this:\n"
            putStrLn (drawTracker (mapTom z) (mapJerry z))
            putStrLn "Goodbye."
            return ()

    goJerry :: Game -> IO()
    goJerry z = do
      putStrLn "You do not feel any movement."
      go z

main = repl