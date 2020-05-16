import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par (NFData) 

import Control.Monad.Par as P
import System.Random
import Data.List
import Criterion.Main

main :: IO ()
main = benchTasks

parQsort :: (NFData a, Ord a) => [a] -> [a]
parQsort [] = []
parQsort (x:xs) = runPar $ do
  less <- spawnP . parQsort $ filter (< x) xs
  bigger <- spawnP . parQsort $ filter (>= x) xs
  l <- get less 
  g <- get bigger 
  -- [l,g] <- mapM get [less, bigger]
  return $ l ++ [x] ++ g

seqQsort :: Ord a => [a] -> [a]
seqQsort [] = []
seqQsort (x:xs) = seqQsort less ++ [x] ++ seqQsort bigger
  where
    less = filter (< x) xs
    bigger = filter (>= x) xs

dParQsort :: (NFData a, Ord a) => Int -> [a] -> Par [a]
dParQsort _ [] = return []
dParQsort 0 xs = return $ seqQsort xs
dParQsort d (x:xs) = do
  less <- spawn $ dParQsort (d-1) $ filter (< x) xs
  bigger <- spawn $ dParQsort (d-1) $ filter (>= x) xs
  l <- get less
  g <- get bigger
  return $ l ++ [x] ++ g

benchTasks :: IO ()
benchTasks = do
  seed <- newStdGen
  let len = 100000
      rl = randomlist len seed
     
      qs = bgroup "Quicksort"
        [bench "Sequential" (nf seqqsort rl)
        ,bench "Parallel" (nf parqsort  rl) 
        [bench "Parallel with depth" (nf (runPar . dParQsort 6) rl)]
       
  defaultMain [qs]



randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)