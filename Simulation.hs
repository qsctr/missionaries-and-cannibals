module Simulation where

import Data.List
import Data.Function

data Boat = L | R deriving (Eq, Show, Read)
type State = ((Int, Int), (Int, Int), Boat)
type StateWithLog = (State, [String])

search :: (Num a, Ord a) => [StateWithLog] -> State -> a -> a -> Maybe StateWithLog
search paths goal limit iter
  | iter >= limit = Nothing
  | Just res <- find ((== goal) . fst) paths = Just res
  | otherwise = search (nubBy ((==) `on` fst) (paths >>= step)) goal limit $ iter + 1

step :: StateWithLog -> [StateWithLog]
step x = filter ok $ map (move x) [(1, 0), (2, 0), (1, 1), (0, 2), (0, 1)]
  where ok (((ml, cl), (mr, cr), boat), _) =
          all (>= 0) [ml, cl, mr, cr] && (ml >= cl || ml == 0) && (mr >= cr || mr == 0)
        move (((ml, cl), (mr, cr), boat), steps) (mb, cb)
          | boat == L = next ((ml - mb, cl - cb), (mr + mb, cr + cb), R)
          | boat == R = next ((ml + mb, cl + cb), (mr - mb, cr - cb), L)
          where next s = (s, steps ++ ["Move " ++ show mb ++ " missionar"
                  ++ (if mb == 1 then "y" else "ies") ++ " and " ++ show cb ++ " cannibal"
                  ++ (if cb == 1 then "" else "s") ++ " to the "
                  ++ (if boat == L then "right" else "left"), "New state is " ++ show s])