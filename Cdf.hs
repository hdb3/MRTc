module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)

main = do
  args <- getArgs
  let n = read (args !! 1) :: Int
  f <- C.readFile (head args)
  -- trivial n f
  fullFat n f

trivial n f = do
  args <- getArgs
  let l = C.lines f !! m
      x = index n l
      m = read (args !! 2) :: Int
  C.putStrLn l
  print x

fullFat n f = mapM_ (extract n) (C.lines f)

index n l = readInt $ (!!n) $ C.words l
readInt s = n where Just (n,_) = C.readInt s
extract n l = print ( index n l)
