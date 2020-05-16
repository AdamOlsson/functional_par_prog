import System.Random
import Control.Parallel.Strategies
import Control.Monad.Par


----------------------------------------------------------------------------
-- # Merge function used in the conquer step in divide and conquer
----------------------------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

-- What I have tried fix the fizzled sparks:
-- Add granularity
-- Evaluate left half in par and right in seq (before any divide call)
-- For each divide call, evaluate left half in par and right in seq

-----------------------------------------------------------------------------
-- Merge sort using Par
-----------------------------------------------------------------------------
mergeSort1 :: (Ord a, NFData a) => [a] -> [a]
mergeSort1 [] = []
mergeSort1 [x] = [x]
mergeSort1 xs = runPar $ do
    i <- new 
    j <- new
    fork $ put i $ mergeSort1 as
    fork $ put j $ mergeSort1 bs
    as' <- get i
    bs' <- get j
    return $ merge as' bs'
  where (as,bs)  = splitAt (length xs `div` 2) xs
  
-----------------------------------------------------------------------------
-- # Merge sort using rpar and rseq
-----------------------------------------------------------------------------
mergeSort2 :: (Ord a, NFData a) => Int -> [a] -> [a]
mergeSort2 _ []  =  []
mergeSort2 _ [x] =  [x]
mergeSort2 d xs | d <= 0 = merge l1 r1
                | otherwise = merge l1 r1 `using` strat
  where half    = (length xs `div` 2)
        (l0,r0) = splitAt half xs
        d'      = d-1
        l1      = mergeSort2 d' l0
        r1      = mergeSort2 d' r0
        strat x = do r l1; r r1; return x
          where r | half < 2  = rdeepseq
                  | otherwise = rpar

-----------------------------------------------------------------------------
-- # Merge sort using rpar and rseq that splits the first conquer call
-- so that the left half is evaluated in parallel and the right in sequence.
-----------------------------------------------------------------------------

parMergeSort3 :: (Ord a, NFData a) => Int -> [a] -> [a]
parMergeSort3 _ []  =  []
parMergeSort3 _ [x] =  [x]
parMergeSort3 d xs | d <= 0    = merge l1 r1
                   | otherwise = merge l1 r1 `using` strat
  where half    = (length xs `div` 2)
        (l0,r0) = splitAt half xs
        d'      = d-1
        l1      = parMergeSort3 d' l0
        r1      = parMergeSort3 d' r0
        strat x = do r l1; r r1; return x
          where r | half < 2  = rdeepseq
                  | otherwise = rpar

seqMergeSort3 :: (Ord a, NFData a) => [a] -> [a]
seqMergeSort3 []  =  []
seqMergeSort3 [x] =  [x]
seqMergeSort3 xs = merge l1 r1 `using` strat
  where half    = (length xs `div` 2)
        (l0,r0) = splitAt half xs
        l1      = seqMergeSort3 l0
        r1      = seqMergeSort3 r0
        strat x = do rdeepseq l1; rdeepseq r1; return x

mergeSort3 :: (Ord a, NFData a) =>Int -> [a] -> [a]
mergeSort3 d xs = runEval $ do
  l1 <- rpar nf1
  r1 <- rdeepseq nf2
  return $ merge l1 r1
  where half    = (length xs `div` 2)
        (l0,r0) = splitAt half xs
        nf1     = parMergeSort3 d l0
        nf2     = seqMergeSort3 r0

-----------------------------------------------------------------------------
-- # Merge sort using rpar using a Tree implementation.
-----------------------------------------------------------------------------

data Tree a = Empty | Branch (Tree a) (Tree a) | Leaf a
  deriving (Eq, Ord)

instance Show a => Show (Tree a) where
  show (Empty)        = ""
  show (Branch t1 t2) = show t1 ++ " " ++ show t2 
  show (Leaf a)       = show a

fromList :: [a] -> Tree a
fromList []  = Empty
fromList [x] = Leaf x
fromList xs  = Branch t t'
  where half = (length xs `div` 2)
        (l,r) = splitAt half xs
        t  = fromList l
        t' = fromList r

mergeSortTree' :: (Ord a) => Tree a -> [a]
mergeSortTree' Empty          = []
mergeSortTree' (Leaf v)       = [v]
mergeSortTree' (Branch t1 t2) = merge l r `using` strat
  where
    l = mergeSortTree' t1
    r = mergeSortTree' t2
    strat x = do rpar l; rpar r; return x

mergeSortTree :: (Ord a) => [a] -> [a]
mergeSortTree = mergeSortTree' . fromList


-------------------------------------------------------------------------------
-- # Merge sort using Divide and Conquer skeleton
-------------------------------------------------------------------------------

dq :: (a -> b) -> a -> (a -> Bool) -> (b -> b -> b) -> (a -> Maybe(a,a)) -> b
dq f arg thresh conquer divide = go arg
  where 
    go arg = 
      case divide arg of
        Nothing      -> f arg
        Just (l0,r0) -> conquer l1 r1 `using` strat
          where 
            l1 = go l0
            r1 = go r0
            strat x = do r l1 ; r r1; return x
              where r | thresh arg = rseq
                      | otherwise  = rpar

divide :: [a] -> Maybe([a], [a])
divide xs = case half >= 2 of
    True -> Just $ splitAt half xs
    False -> Nothing
  where half = length xs `div` 2

mergeSort_dq :: Ord a => Int -> [a] -> [a]
mergeSort_dq threshold arg = dq id arg thresh merge divide
  where 
    thresh :: [a] -> Bool
    thresh  xs = length xs < threshold

-------------------------------------------------------------------------------
-- # Main
-------------------------------------------------------------------------------
crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main :: IO()
main = do
  let xs =  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )
  let rs = crud xs

  -- print $ sum $ mergeSort1 xs     -- Par
  print $ sum $ mergeSort2 10 rs  -- rpar and rseq
  -- print $ sum $ mergeSort3 8 rs   -- rpar and rdseq before first divide
  -- print $ sum $ mergeSortTree xs  -- Tree implementation using rpar and rseq
  -- print $ sum $ mergeSort_dq 2 xs -- Using skeletons
