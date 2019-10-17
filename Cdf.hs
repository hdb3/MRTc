module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap as Map
import Data.List(foldl')

main = do
  args <- getArgs
  let n = read (args !! 1) :: Int
  f <- C.readFile (head args)
  let nx = map (index n) (C.lines f)
  let f m v = Map.insertWith (+) v 1 m
      m = foldl' f Map.empty nx
      lm = Map.toAscList m
  mapM_ print lm

index n l = readInt $ (!!n) $ C.words l
readInt s = n where Just (n,_) = C.readInt s
