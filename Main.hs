module Main where

import Simulation
import Text.Read

defaultStart = ((0, 0), (3, 3), R)
defaultGoal = ((3, 3), (0, 0), L)
defaultLimit = 1 / 0

main = do
  putStrLn $ "State is in the format of ((left missionaries, left cannibals), "
    ++ "(right missionaries, right cannibals), boat location either L or R)"
  putStrLn $ "Enter starting state. Default is " ++ show defaultStart
  start <- inputWithDefault defaultStart
  putStrLn $ "Start state is " ++ show start
  putStrLn $ "Enter goal state. Default is " ++ show defaultGoal
  goal <- inputWithDefault defaultGoal
  putStrLn $ "Goal state is " ++ show goal
  putStrLn $ "Enter iteration limit. Default is " ++ show defaultLimit
  limit <- inputWithDefault defaultLimit
  putStrLn $ "Iteration limit is " ++ show limit
  putStrLn "Searching..."
  mapM_ putStrLn $ maybe ["No solution found within the iteration limit"]
    (("Solution found, steps are shown below" :) . snd) $ search [(start, [])] goal limit 0
  where inputWithDefault def = maybe def id . readMaybe <$> getLine