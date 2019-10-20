module Main where
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment(getArgs)
import qualified Data.IntMap as Map
import Data.List(foldl',intercalate)
import System.IO(openFile,IOMode(WriteMode),hPutStrLn,hClose)

{-

  multi-column design
  each line read returns a fixed length list of int:
  we can combine each update with a list of maps
-}
main = do
  args <- getArgs
  let n = read (args !! 1) :: Int
  f <- C.readFile (head args)
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
      aggregates = zip columnNames (map Map.toAscList parsedContent)
  putStrLn $ "read the following columns: " ++ unwords columnNames
  mapM_ processCumulative aggregates
  mapM_ processPad aggregates

processCumulative (name,rows) = do 

    let 
        accumulate = go 0 where
            go z ( (k,c):kcx) | null kcx = [(k,c,z+c)]
                              | otherwise = (k,c,z+c) : go (z+c) kcx

        cumulativeRows :: [(Int,Int,Int)]
        cumulativeRows = accumulate rows

        (_,_,sampleCount) = last cumulativeRows
        sampleCountFloat = fromIntegral sampleCount

        normedSamples :: [(Int,Int,Int,Double)]
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
  design for inserting zeroes for empty slots.
  This probably only makes sense for the simple accumulated values,
  though by adding the slots at the first stage the other derivateives come for free...

  Whilst there may be an obvious fold based solution, it is not very obvious,
  and this even more basic method is simple.
  Assume the input is a sorted list on keys: [(key,count)]
  scan the list with an incrementing index.  Consume the list head when the index is equal the key,
other wise produce a zero count tuple.
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