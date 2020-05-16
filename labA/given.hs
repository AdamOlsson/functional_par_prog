import Data.List
import System.Random
import Criterion.Main
import Control.Parallel 
import Control.Parallel.Strategies hiding (evalList, parList)
import Control.Monad.Par hiding (parMap, spawn)

--ghc -O2 -threaded -rtsopts -eventlog -feager-blackholing

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int

-------------------------------------------
-- Using par and pseq
-------------------------------------------

pmapA :: (a -> b) -> [a] -> [b]
pmapA f []     = []
pmapA f (x:xs) = par nf1 (pseq nf2 (nf1:nf2))
  where nf1  = f x
        nf2 = pmapA f xs

jackknifeA :: ([a] -> b) -> [a] -> [b]
jackknifeA f = pmapA f . resamples 500

-------------------------------------------
-- Using rpar and rseq from the Eval monad
-------------------------------------------

pmapB :: (a -> b) -> [a] -> Eval [b]
pmapB f []     = return []
pmapB f (x:xs) = do
  nf1 <- rpar (f x)
  nf2 <- pmapB f xs
  return (nf1:nf2)


jackknifeB :: ([a] -> b) -> [a] -> [b]
jackknifeB f = runEval . pmapB f . resamples 500

-- To compare with built-in parMap
jackknifeB2 :: ([a] -> b) -> [a] -> [b]
jackknifeB2 f = parMap rpar f . resamples 500

-------------------------------------------
-- Using strategies
-------------------------------------------

evalList :: Strategy a -> Strategy [a]
evalList _ []     = return []
evalList s (x:xs) = do
  x' <- s x
  xs' <- evalList s xs
  return (x':xs')

parList :: Strategy a -> Strategy [a]
parList s = evalList (rpar `dot` s)

jackknifeC :: (a -> b) -> [a] -> [b]
jackknifeC f xs = (map f xs) `using` (parList rseq)

-------------------------------------------
-- Using Par monad
-------------------------------------------

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do 
  i <- new
  fork $ do x <- p; put i x
  return i

pmapD :: NFData b => (a -> b) -> [a] -> [b]
pmapD f xs = runPar $ do
  ibs <- mapM (spawn . return . f) xs
  mapM get ibs 

jackknifeD :: NFData b => ([a] -> b) -> [a] -> [b]
jackknifeD f = pmapD f . resamples 500

-------------------------------------------
-- Default code provided
-------------------------------------------

mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))


jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]


main :: IO ()
main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)
  let j = jackknife mean rs :: [Float]

  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)

  -- Examples 
  -- print $ sum $ jackknifeA mean rs                 -- par and pseq
  -- print $ sum $ jackknifeB mean rs                 -- Eval Monad
  -- print $ sum $ jackknifeB2 mean rs                -- Eval Monad

  -- print $ sum $ jackknifeC mean $ resamples 500 rs -- Strategies
  -- print $ sum $ jackknifeD mean rs                 -- Par Monad


  -- The benchmark is left if the TA would like to run the code using it.
  -- Note: The import of Criterion is also in comments

  defaultMain
        [
        --  bench "jackknife"  (nf (jackknife   mean) rs),
        --  bench "jackknifeA" (nf (jackknifeA  mean) rs)
        --  bench "jackknifeA" (nf (jackknifeA3 mean 100 ) rs)  

         bench "jackknifeB" (nf (jackknifeB  mean) rs)
        --  bench "jackknifeB2"(nf (jackknifeB2 mean) rs)
        --  bench "jackknifeC" (nf (jackknifeC  mean) $ resamples 500 rs)
        --  bench "mergeSort"  (nf mergeSort rs)       
        ]
