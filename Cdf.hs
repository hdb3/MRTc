module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap as Map
import Data.List(foldl',intercalate)

main = do
  args <- getArgs
  let n = read (args !! 1) :: Int
  f <- C.readFile (head args)
  let nx = map (index n) (C.lines f)
  let f m v = Map.insertWith (+) v 1 m
      m = foldl' f Map.empty nx
      lm = Map.toAscList m
      alm = accumulate lm
      alm2 = accumulate2 lm
      samples = snd $ last alm
      ialm = map (\(k,c) -> (k,(samples-c)/samples)) alm
      ialm2 :: [(Int,Int,Int,Double)]
      ialm2 = map (\(k,c,z) -> (k,c,z,(samples-z)/samples)) alm2
      put (k,v) = putStrLn $ show k ++ " " ++ show v
      --put2 (k,v,agg,invagg) = putStrLn $ intercalate " " [show k, show v, show agg, show invagg]
      put2 (k,v,agg,invagg) = putStrLn $ unwords [show k, show v, show agg, show invagg]
  --mapM_ put ialm
  mapM_ put2 ialm2

index n l = readInt $ (!!n) $ C.words l
readInt s = n where Just (n,_) = C.readInt s

accumulate = go 0 where
    go z ( (k,c):kcx) | null kcx = [(k,z+c)]
                      | otherwise = (k,z+c) : go (z+c) kcx

accumulate2 = go 0 where
    go z ( (k,c):kcx) | null kcx = [(k,c,z+c)]
                      | otherwise = (k,c,z+c) : go (z+c) kcx
