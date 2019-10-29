module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.Map.Strict as Map
import Data.List(foldl',intercalate,sortOn,maximumBy)
import Data.Maybe(fromJust)

{-
-- SumSD
-- sum multiple rows with the same control parameters
-- uses 1 (or more ?) columns as a key for equivalence
-- aggregates mean/min/max/sd/count/rate
--  adds another set of columns for rate and conditioning (mean/min/max/SD)
-- rate aggregation calculations done on the inverse

-- default assumption of columns is:
-- LOGTEXT TEST TIME SENDERS CONDITIONING MEAN MAX MIN STDDEV TABLESIZE GROUPSIZE MAXBURSTCOUNT REPEAT WINDOW RATECOUNT SINGLERATE MULTIRATE
-- 0       1    2    3       4            5    6   7   8      9         10        11            12     13     14        15         16
-- i.e. 0 : key
--      4 : conditiong value
--      5,6,7,8,12 mean,max,min,SD,count
--      15 : rate value 1
--      16 : rate value 2
--
design

partition lines based on the key - requires a function from lines to keys - this simply an index into the word list
  the resulting map holds just lines, not words....

each map entry is processed alike, the key provides the first field in the aggregate
map entry processing is a fold over the line list which passes the word list in full
for raw values the return value is a tuple (count,min,max,sum,sqsum)
and the subsequent processing yields (mean,sd,min,max)
For intermediate aggregates the result is the same but themethod different (sum is count * mean, sqsum is calculated
by the inverse of double sd = sqrt ( (count * sqsum) - (sum * sum) ) / count;
   count * sd = sqrt ( (count * sqsum) - (sum * sum) )
  (count * sd)^2 = (count * sqsum) - (sum * sum)
  (count * sd)^2  + sum * sum = count * sqsum
  ((count * sd)^2  + sum * sum) /count = sqsum
)
-}

main = mainMap

mainMap = do
    content <- C.lines <$> getContent
    let
        f' m line = let (k,v) = C.break (== ' ') line in Map.insertWith (++) k [C.tail v] m
        m = foldl' f' Map.empty (tail content)
        lm = Map.toAscList m
    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"
    putStrLn $ "the list has " ++ show (length lm) ++ " elements"
    -- mapM_ (\(k,l) -> putStrLn $ C.unpack k ++ " : " ++ show (length l)) lm
    n <- getArgInt
    mapM_ (\(k,l) -> putStrLn $ C.unpack k ++ " : " ++ show ( sumColumn n l)) lm
    -- mapM_ (\(k,l) -> putStrLn $ C.unpack k ++ " : " ++ show ( simpleRead n l)) lm

sumColumn :: Int -> [C.ByteString] -> (Double,Double,Double,Double)
sumColumn n = simpleCollect . simpleRead n

readDouble :: C.ByteString -> Double
readDouble = read . C.unpack

readInt :: C.ByteString -> Int
readInt s = fst $ fromJust $ C.readInt s

readInts :: C.ByteString -> [Int]
readInts s = map readInt $ C.words s

simpleCollect :: (Int,Double,Double,Double,Double) -> (Double,Double,Double,Double)
simpleCollect (count,mn,mx,sum,sqsum) =
    let count' = fromIntegral count
    in ( sum / count'
       ,  sqrt ( (count' * sqsum) - (sum * sum) )  / count'
       , mn
       , mx
       )

simpleRead :: Int -> [C.ByteString] -> (Int, Double, Double, Double, Double)
simpleRead n lines = simpleSum $ map ( readDouble . (!! n) . C.words ) lines

-- simpleSum :: [Int] -> (Int,Int,Int,Int,Int)
simpleSum = foldl' simpleSum' (0,-1,0,0,0)
    where
        simpleSum' (count,mn,mx,sum,sqsum) v =
            (count+1
            , if -1 == mn then v else min mn v
            , max mx v
            ,sum + v
            ,sqsum + v * v
            )

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