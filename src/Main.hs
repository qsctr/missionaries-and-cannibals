module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Function
import System.Console.ANSI
import System.IO
import Text.Read

data Boat = L | R deriving (Eq, Show, Read)
type Config = (Int, Int)
type State = (Config, Config, Boat)
type StateWithSteps = (State, (Config, IO ()))
type BoatSize = Int

main :: IO ()
main = do
  let ask name def = do
        putStrLn $ "Enter " ++ name ++ ". Default is " ++ show def
        answer <- maybe def id . readMaybe <$> getLine
        putStrLn $ name ++ " is " ++ show answer
        return answer
      exit = do
        putStrLn "Press enter to exit"
        void getLine
  (start, totalM, totalC) <- do
    ml <- ask "left missionaries (start)" 0
    cl <- ask "left cannibals (start)" 0
    mr <- ask "right missionaries (start)" 3
    cr <- ask "right cannibals (start)" 3
    boat <- ask "boat side (start) (L or R)" R
    return (((ml, cl), (mr, cr), boat), ml + mr, cl + cr)
  (goalL, goalR) <- do
    ml <- ask "left missionaries (goal)" 3
    cl <- ask "left cannibals (goal)" 3
    mr <- ask "right missionaries (goal)" 0
    cr <- ask "right cannibals (goal)" 0
    if ml + mr /= totalM then do
      putStrLn "Missionary numbers don't match"
      exit
      undefined
    else if cl + cr /= totalC then do
      putStrLn "Cannibal numbers don't match"
      exit
      undefined
    else return ((ml, cl), (mr, cr))
  boatSize <- ask "boat size" 2
  limit <- ask "iteration limit" $ 1 / 0
  putStrLn "Searching..."
  let search :: [StateWithSteps] -> Double {- 1 / 0 :: Double -} -> Maybe (IO ())
      search paths iter
        | iter > limit = Nothing
        | Just (_, (_, steps)) <- find isGoal paths = Just steps
        | otherwise = search ((nubBy ((==) `on` fst)) (paths >>= step)) $ iter + 1
        where isGoal :: StateWithSteps -> Bool
              isGoal ((l, r, _), _) = l == goalL && r == goalR
              boats :: [Config]
              boats = boatConfigs boatSize
              boatConfigs :: BoatSize -> [Config]
              boatConfigs n
                | n <= 0 = []
                | otherwise = [ (x, n - x) | x <- [0..n] ] ++ boatConfigs (n - 1)
              valid :: StateWithSteps -> Bool
              valid (((ml, cl), (mr, cr), _), _) = all (>= 0) [ml, cl, mr, cr]
                && (ml >= cl || ml == 0) && (mr >= cr || mr == 0)
              step :: StateWithSteps -> [StateWithSteps]
              step (((ml, cl), (mr, cr), boat), (prevBoat, prevSteps)) =
                  filter valid $ map move $ delete prevBoat boats
                  where move (mb, cb)
                          | bl = addSteps ((ml - mb, cl - cb), (mr + mb, cr + cb), R)
                          | br = addSteps ((ml + mb, cl + cb), (mr - mb, cr - cb), L)
                          where bl = boat == L
                                br = boat == R
                                addSteps state@((ml', cl'), (mr', cr'), _) =
                                  (,) state $ (,) (mb, cb) $ do
                                    prevSteps
                                    colorM
                                    printM ml
                                    colorC
                                    printC cl
                                    colorR
                                    putRep (if bl then 0 else 10) '.'
                                    putChar '['
                                    putRep boatSize '_'
                                    putChar ']'
                                    putRep (if br then 0 else 10) '.'
                                    colorM
                                    printM mr
                                    colorC
                                    printC cr
                                    hFlush stdout
                                    threadDelay 500000 -- microseconds
                                    resetLine
                                    forM_ (if bl then [0..10] else [10,9..0]) $ \i -> do
                                      colorM
                                      printM (if bl then ml' else ml)
                                      colorC
                                      printC (if bl then cl' else cl)
                                      colorR
                                      putRep i '.'
                                      putChar '['
                                      colorM
                                      putRep mb '@'
                                      colorC
                                      putRep cb '#'
                                      colorR
                                      putRep (boatSize - mb - cb) '_'
                                      putChar ']'
                                      putRep (10 - i) '.'
                                      colorM
                                      printM (if br then mr' else mr)
                                      colorC
                                      printC (if br then cr' else cr)
                                      hFlush stdout
                                      threadDelay 150000
                                      resetLine
                                    colorM
                                    printM ml'
                                    colorC
                                    printC cl'
                                    colorR
                                    putRep (if bl then 10 else 0) '.'
                                    putChar '['
                                    putRep boatSize '_'
                                    putChar ']'
                                    putRep (if br then 10 else 0) '.'
                                    colorM
                                    printM mr'
                                    colorC
                                    printC cr'
                                    putStrLn ""
              resetLine = clearLine >> setCursorColumn 0
              printM m = putRep m '@' >> putRep (totalM - m) ' '
              printC c = putRep c '#' >> putRep (totalC - c) ' '
              putRep n c = putStr $ replicate n c
      colorM = setSGR [SetColor Foreground Vivid Blue]
      colorC = setSGR [SetColor Foreground Vivid Red]
      colorR = setSGR [Reset]
  case search [(start, ((0, 0), return ()))] 1 of
    Nothing -> putStrLn "No solution found within the iteration limit"
    Just steps -> do
      putStrLn "Solution found"
      colorM
      putStrLn "@ = missionary"
      colorC
      putStrLn "# = cannibal"
      steps
      colorR
      exit