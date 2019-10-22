module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap.Strict as Map
import Data.List(foldl',intercalate)
import System.IO(openFile,IOMode(WriteMode),hPutStrLn,hClose)

{-

  multi-column design
  each line read returns a fixed length list of int:
  we can combine each update with a list of maps
-}
main = do
  args <- getArgs
  aggregate <- getAggregate (head args)
  mapM_ processCumulative aggregate
  mapM_ processPad aggregate
  mapM_ processPercentiles aggregate

getAggregate :: String -> IO [(String, [(Int, Int)])]
getAggregate fn = do
    f <- C.readFile fn
    let (header:content) = C.lines f
        columnNames = map C.unpack $ C.words header
        readInt s = n where Just (n,_) = C.readInt s
        fieldList = map (map readInt . C.words) content
        emptyMaps = replicate (length columnNames) (Map.empty :: Map.IntMap Int)
        updater :: Map.IntMap Int -> Int -> Map.IntMap Int
        updater m n = Map.insertWith (+) n 1 m
        updater2 :: [Map.IntMap Int] -> [Int] -> [Map.IntMap Int]
        updater2 = zipWith updater
        parsedContent = foldl updater2 emptyMaps fieldList
        aggregate = zip columnNames (map Map.toAscList parsedContent)
    putStrLn $ "read the following columns: " ++ unwords columnNames
    return aggregate

accumulate :: [(Int,Int)] -> [(Int,Int,Int)]
accumulate = go 0 where
go z ( (k,c):kcx) | null kcx = [(k,c,z+c)]
                  | otherwise = (k,c,z+c) : go (z+c) kcx

processCumulative (name,rows) = do 

    let 
        cumulativeRows = accumulate rows
        (_,_,sampleCount) = last cumulativeRows
        sampleCountFloat = fromIntegral sampleCount
        normedSamples = map (\(k,c,z) -> (k,c,z,(sampleCountFloat - fromIntegral z)/sampleCountFloat)) cumulativeRows
    
    putStr $ "processColumn: " ++ name
    putStr $ "read " ++ show (length rows) ++ " rows, "
    putStrLn $ show sampleCount ++ " samples"
    let fn = name++".ssv"
        printRow f (k,v,agg,invagg) = hPutStrLn f $ unwords [show k, show v, show agg, show invagg]
    outfile <- openFile  fn WriteMode
    mapM_ (printRow outfile) normedSamples
    putStrLn $ "written to " ++ fn
    hClose outfile


{-
  Pad: inserting zeroes for empty slots.
  This probably only makes sense for simple accumulated values,
  though by adding the slots at the first stage the other derivatives come for free...
  Assume the input is a sorted list on keys: [(key,count)]
  - scan the list with an incrementing index.  Consume the list head when the index is equal the key,
  - otherwise produce a zero count tuple.
-}

pad :: [(Int,Int)] -> [(Int,Int)]
pad = go 0 where
    go n ((k,c):kvs) | null kvs = [(k,c)]
                     | n == k = (k,c): go (n+1) kvs
                     | otherwise = (n,0) : go (n+1) ((k,c):kvs)


processPad (name,rows) = do 

   let fn = name++".pad.ssv"
       printRow f (k,v) = hPutStrLn f $ unwords [show k, show v]
   outfile <- openFile  fn WriteMode
   mapM_ (printRow outfile) (pad rows)
   putStrLn $ "written to " ++ fn
   hClose outfile

percentile :: (Int,Int) -> [(Int,Int)] -> Int
percentile (m,n) kvs = scan threshold kvs where
  max = snd $ last kvs
  threshold = max - (max * m `div` n)
  scan t ((k,v):kvs) | null kvs = k
                     | v < t = scan t kvs
                     | otherwise = k


processPercentiles (name,rows) = do
    putStr $ "percentiles: " ++ name
    let breaks = [ ("median",(1,2))
                 , ("90%",(1,10))
                 , ("99%",(1,100))
                 , ("99.9%",(1,1000))
                 , ("max",(0,1))
                 ]
        cumulativeRows = map (\(a,_,b) -> (a,b)) $ accumulate rows         
        percentiles = map (\(label,ratio) -> (label,percentile ratio cumulativeRows)) breaks
    putStrLn $ unwords $ map show percentiles
