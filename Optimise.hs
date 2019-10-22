{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap.Strict as Map
import Data.List(foldl',intercalate)
import Data.Maybe(fromJust)

main = do
    content <- getContent
    !n <- getArgInt
    let
        lines = tail $ C.lines content
        getInt = (!! n) . readInts
        f' m !line = Map.insertWith (+) (getInt line) 1 m
        m = foldl' f' Map.empty lines
        l = foldl' (\a b -> a + getInt b) 0 lines
        lm = Map.toAscList m
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"
    -- putStrLn $ "the map has " ++ show l ++ " elements"

readInt :: C.ByteString -> Int
readInt !s = fst $ fromJust $ C.readInt s

readInts :: C.ByteString -> [Int]
readInts !s = map readInt $ C.words s

getTuple ((!a) : (!b) :_) = (a,b)
readTuple = getTuple . readInts
readTuples = map readTuple . C.lines

getContent = do
    args <- getArgs
    C.readFile (head args)

getArgInt :: IO Int
getArgInt = do
    args <- getArgs
    return ( read (args !! 1) :: Int )

{-
main = do
    content <- getContent
    let tuples = readTuples content
        f m (k,v) = Map.insertWith (+) k v m
        m = foldl' f Map.empty tuples
        lm = Map.toAscList m
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"
-}
