{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap.Strict as Map
import Data.List(foldl',intercalate)
import Data.Maybe(fromJust)

--main = mainMap
main = mainTuple

mainMap = do
    content <- getContent
    n <- getArgInt
    let
        lines = tail $ C.lines content
        getInt = (!! n) . readInts
        f' m line = Map.insertWith (+) (getInt line) 1 m
        m = foldl' f' Map.empty lines
        lm = Map.toAscList m
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"
    putStrLn $ "the list has " ++ show (length lm) ++ " elements"

readInt :: C.ByteString -> Int
readInt s = fst $ fromJust $ C.readInt s

readInts :: C.ByteString -> [Int]
readInts s = map readInt $ C.words s

getTuple (a : b :_) = (a,b)
getTuple' l | 2 < length l = (head l, l !! 2)
--getTuple l | 3 > length l = (a,b)
            | otherwise = (-1,-1)
    where a = head l
          b = head (drop 2 l)

readTuple = getTuple' . readInts
readTuples = map readTuple . C.lines

getContent = do
    args <- getArgs
    C.readFile (head args)

getArgInt :: IO Int
getArgInt = do
    args <- getArgs
    return ( read (args !! 1) :: Int )

mainTuple = do
    content <- C.lines <$> getContent
    --putStrLn $ unlines $ map (show . getTuple . readInts) content
    let tuples = map readTuple content
        f m (k,v) = Map.insertWith (+) k v m
        m = foldl' f Map.empty tuples
        lm = Map.toAscList m
    print lm
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"
