module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.Map.Strict as Map
import Data.List(foldl',intercalate,sortOn,maximumBy,(\\))
import Data.Maybe(fromJust)
import Text.Printf
import System.IO
import System.Process
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

For intermediate aggregates the result is the same but the method different (sum is count * mean, sqsum is calculated
by the inverse of double sd = sqrt ( (count * sqsum) - (sum * sum) ) / count;
   count * sd = sqrt ( (count * sqsum) - (sum * sum) )
  (count * sd)^2 = (count * sqsum) - (sum * sum)
  (count * sd)^2  + sum * sum = count * sqsum
  ((count * sd)^2  + sum * sum) /count = sqsum
)

Design for graph defintion

The input records have regularly ordered lists of strings as tags.
The selection process consists of defining the tag indices to be used to define a graph.
The other indices should be held fixed for a single graph.
The result is a set of graphs, whose defined by the parameters which were kept fixed.
The gnult polt format allows up to three levels of hierarchy for a plot.
The first two levels can be labeled by setting the row and column headers in one file.
The third level requires multiple  plot statmenets and input files for each group.

Formatting the selected records
Assume that the records have been reduced to a list like this:
[(String,([String,Data]))
and that the inner keys are consistent, so that
([cols],[(row,[rec]))   :: [(([String],[(String,Data)])
could be derived by simpling picking off the keys from the first row.
Then the desired output is one line:
  "null" : cols
followed by 'n' rows of:
   row : recs

and the rendering is trivial. 

Generating the buckets
Each bucket corresponds to a single plot.
For now assume two level hierarchy, i.e. there are rows and columns corresponding to two tag positions,
and all remaining tags are grouped to define the graph title / content.
First the record keys should be split to enable the partitioning: given two indices which are to be used for row and column,
we remove these elements to a separate tuple field, and leave the remainder intact.

module Nest where

-}
-- The function signature is:
f :: Int -> Int -> ([String],a) -> ([String],(String,String,a))
f i j (sx,a) = (sx',(si,sj,a)) where (sx',si,sj) = f' i j sx
--  From this we build a map which will contain buckets of form [(String,String,a)].
--  The inner function is:
f' :: Int -> Int -> [String] -> ([String],String,String)
--  and is this:
f' i j sx = (rx,xi,xj) where
    l = length sx
    xi = sx !! i
    xj = sx !! j  
    rxi = [0..l-1] \\ [i,j]
    rx = foldl' (\a i -> a ++ [sx !! i]) [] rxi

h z = Map.toAscList m where
    m = foldl' f' Map.empty z 
    f' m (k,v) = Map.insertWith (++) k [v] m 
    
{-
Rendering the buckets
We want to generate [[a]].
Build a Map over the outer key, then reduce to list, for a list of (outer key, [(inner key , a)]) tuples.
The row labels are the outer keys.
Get a set of column labels by mapping fst over the head of the list.
Get the content by mapping snd over the list.

-}


g :: [(String,String,a)] -> ([String],[String],[[a]])
  
g z = (rows,columns,axx) where
   
    m = foldl' f' Map.empty z 
    f' m (r,c,v) = Map.insertWith (++) r [(c,v)] m 
    l = Map.toAscList m 
    rows = map fst l 
    columns = map fst $ snd $ head l 
    axx = map ( map snd. snd ) l 

render :: Int -> ([String],[String],[[String]]) -> [String]
render n (rows,columns,axx) = header : body where
    axx' = map ( map ( unwords . take n . words)) axx -- drop the unwanted rsd/min/max columns
    header = "<null>" ++ concatMap (\c -> concat $ replicate n ( ' ' : c)) columns
    -- header = "- " ++ unwords $ concatMap ( replicate 2) columns
    body = map (\(a,b) -> a ++ " " ++ unwords b) ( zip rows axx' )

main = mainMap

mainMap = do
    args <- getArgs
    let (i,j) = if 3 > length args then (0,1) else (read $ args !! 1 , read $ args !! 2) :: (Int,Int)
    let bn = if 3 > length args then 0 else read $ args !! 3 :: Int
    content <- C.lines <$> getContent
    let
        f' m line = let (k,v) = C.break (== ' ') line in Map.insertWith (++) k [C.tail v] m
        m = foldl' f' Map.empty (tail content)
        lm = Map.toAscList m
        records = makeRecords lm "conditioning" (showCollect . sumColumn 7 ) ++
                  makeRecords lm "ssrt" (showCollect . sumColumn 18 ) ++
                  makeRecords lm "msrt" (showCollect . sumColumn 19 ) ++
                  makeRecords lm "ssbt" (showCollect . simpleCollect . rollupRead )
    -- mapM_ print records
    rawPrint "raw.txt" records

    putStrLn $ "the map has " ++ show (Map.size m) ++ " elements"

    let buckets =  h $ map (f i j) records
        bucketNames = map ( unwords . fst) buckets
        bnns = zipWith (\s n -> show n ++ " \"" ++ s ++ "\"" ) bucketNames [1..]
    if bn > 1 then do
        let bucket = buckets !! (bn-1)
        putStrLn $ "working with bucket " ++ show bn ++ " title " ++ unwords ( fst bucket )
        writeFile "tmp.gpl" $ unlines $ gplot bucket
        putStrLn "output written to \"tmp.gpl\""
        callCommand "gnuplot -p tmp.gpl"
    else do
        let (title1,bucket1) = head buckets
            graph1 = g bucket1
        putStrLn "The following buckets were found: "
        putStrLn $ unlines bnns
        putStrLn $ "working with the first bucket: " ++ unwords title1

        writeFile "tmp.gpl" $ unlines $ gplot (title1,bucket1)

gplot (title,bucket) = datas bucket ++ commands title where

    commands t = [ "set term x11 enhanced"
                 , "set title \"" ++ unwords t ++ "\""
                 , "set boxwidth 0.9 relative"
                 , "set style data histograms"
                 , "set style histogram errorbars"
                 , "set style fill solid 1.0 border lt -1"
                 , "set yrange [0:*]"
                 , "plot for [i=2:*:2] \"$DATA\"  using i:i+1:xtic(1) title columnheader"
                 ]

    datas s = "$DATA << EOD " : render 2 ( g bucket)  ++ ["EOD"]

{-
    -- putStrLn "\ninitial conditioning duration"
    makeFile lm "conditioning" (showCollect . sumColumn 7 )
    -- putStrLn "\nsingle source rate test"
    makeFile lm "ssrt" (showCollect . sumColumn 18 )
    -- putStrLn "\nmultiple source rate test"
    makeFile lm "msrt" (showCollect . sumColumn 19 )
    -- putStrLn "\nsingle source burst test"
    makeFile lm "ssbt" (showCollect . simpleCollect . rollupRead )
-}

makeRecords resultSet name f = 
    map (\(k,l) -> ( words ( substitute '/' ' ' $ trimQuotes ( C.unpack k )) ++ [name] , f l)) resultSet

rawPrint name resultSet = do
    h <- openFile name WriteMode
    mapM_ (\(k,l) -> hPutStrLn h $ ( unwords k ) ++ " " ++ l) resultSet
    hClose h
{-
-}
makeFile resultSet name f = do
    h <- openFile (name ++ ".txt") WriteMode
    mapM_ (\(k,l) -> hPutStrLn h $ substitute '/' ' ' $ trimQuotes ( C.unpack k ) ++ " " ++ f l) resultSet
    hClose h

substitute x y = map (\c -> if c == x then y else c )

trimQuotes = reverse . trim' . reverse . trim' where
    trim' ('"':s) = s
    trim' sx = sx

sumColumn :: Int -> [C.ByteString] -> (Double,Double,Double,Double,Double)
sumColumn n = simpleCollect . simpleRead n

readDouble :: C.ByteString -> Double
readDouble = read . C.unpack

readInt :: C.ByteString -> Int
readInt s = fst $ fromJust $ C.readInt s

readInts :: C.ByteString -> [Int]
readInts s = map readInt $ C.words s

showCollect :: (Double,Double,Double,Double,Double) -> String
--showCollect (a,b,c,d,e) = let p v = show v in unwords $ map p [a,b,c,d,e] 
showCollect (a,b,c,d,e) = let p v = printf "%0.3f" v in unwords $ map p [a,b,c,d,e] 

simpleCollect :: (Int,Double,Double,Double,Double) -> (Double,Double,Double,Double,Double)
simpleCollect (count,mn,mx,sum,sqsum) =
    let count' = fromIntegral count
        mean = sum / count'
        sd = sqrt ( (count' * sqsum) - (sum * sum) )  / count'
        rsdPercent = 100.0 * sd / mean
    in ( mean
       , sd
       , rsdPercent
       , mn
       , mx
       )

simpleRead :: Int -> [C.ByteString] -> (Int, Double, Double, Double, Double)
simpleRead n lines = simpleSum $ map ( readDouble . (!! n) . C.words ) lines

simpleReadR :: Int -> [C.ByteString] -> (Int, Double, Double, Double, Double)
simpleReadR n lines = simpleSum $ map ( reciprocal . readDouble . (!! n) . C.words ) lines

reciprocal x = 1.0 / x

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


rollupRead :: [C.ByteString] -> (Int, Double, Double, Double, Double)
rollupRead lines = rollupSum $ map ( readRollup . C.words ) lines

readRollup :: [C.ByteString] -> (Int, Double, Double, Double, Double)
-- CONDITIONING MEAN MAX MIN STDDEV REPEAT
-- 4            5    6   7   8      12
-- 7            8    9   10  11     15
-- read count , mean , sd and back-calculate to sum and sqsum
readRollup wx = (count,mean,sd,mn,mx) where
    mean = readDouble $ wx !! 8
    mx = readDouble $ wx !! 9
    mn = readDouble $ wx !! 10
    sd = readDouble $ wx !! 11
    count = readInt $ wx !! 15

rollupSum :: [(Int, Double, Double, Double, Double)] -> (Int, Double, Double, Double, Double)
rollupSum = foldl' rollupSum' (0,-1.0,0.0,0.0,0.0)
    where
        rollupSum' :: (Int, Double, Double, Double, Double)
            -> (Int, Double, Double, Double, Double)
            -> (Int, Double, Double, Double, Double)
        rollupSum' (count,mn,mx,sum,sqsum) (count'',mean',sd',mn',mx') =
            let
                count' = fromIntegral count''
                sum' = count' * mean'
                sqsum' = ((count' * sd')^2  + sum' * sum') / count'
            in (count + count''
               , if mn < 0.0 then mn' else min mn mn'
               , max mx mx'
               , sum + sum'
               , sqsum + sqsum'
               )


getContent = do
    args <- getArgs
    C.readFile (head args)

getArgInt :: IO Int
getArgInt = do
    args <- getArgs
    return ( read (args !! 1) :: Int )
