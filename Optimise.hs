module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap.Strict as Map
import Data.List(foldl',intercalate,sortOn,maximumBy)
import Data.Maybe(fromJust)

--main = mainMap
main = do
    mainOptimize
    -- mainTuple
    -- mainTupleMap

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
getTuple' i j l | j < length l = (l !! i, l !! j)
                -- | otherwise = (-1,-1)

readTuple = getTuple . readInts
readTuples = map readTuple . C.lines

getContent = do
    args <- getArgs
    C.readFile (head args)

getArgInt :: IO Int
getArgInt = do
    args <- getArgs
    return ( read (args !! 1) :: Int )

getMap = do
    content <- C.lines <$> getContent
    let tuples = map readTuple content
    return $ foldl' (\m (k,v)-> Map.insertWith (+) k v m) Map.empty tuples

mainTupleMap = do
    -- content <- C.lines <$> getContent
    -- let tuples = map readTuple content
    --    m = foldl' (\m (k,v)-> Map.insertWith (+) k v m) Map.empty tuples
    m <- getMap  
    -- let lm = Map.toAscList m
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"

getList = do
    content <- C.lines <$> getContent
    let tuples = map readTuple content
    return $ sortOn fst tuples
    
mainTuple = do
    l <- getList
    let lx = cumulative $ filter ( (0 /=) . snd ) l
    putStrLn $ unlines $ map show lx

    putStrLn $ "the raw list has " ++ show (length l) ++ " elements"
    putStrLn $ "the filtered list has " ++ show (length lx) ++ " elements"


cumulative :: [(Int,Int)] -> [(Int,Int)]
cumulative = go 0 where
    go z ( (k,c):kcx) | null kcx = [(k,z+c)]
                    | otherwise = (k,z+c) : go (z+c) kcx

mainOptimize = do
    l <- getList
    let lx = cumulative $ filter ( (0 /=) . snd ) l
        (kmax,cmax) = last lx
    putStrLn $ "kmax / cmax = " ++ show kmax ++ "/" ++ show cmax

    putStrLn $ "the raw list has " ++ show (length l) ++ " elements"
    putStrLn $ "the filtered list has " ++ show (length lx) ++ " elements"
    let 
        gain (kn,cn) = (kn, fromIntegral (kmax * cmax) / fromIntegral (kn*cn + kmax * (cmax-cn)) :: Double)
        gains = map gain lx
        maxGain = maximumBy (\(_,a) (_,b) -> compare a b) gains
    -- putStrLn $ unlines $ map show gains
    putStrLn $ "optimal breakpoint and gain is at " ++ show maxGain