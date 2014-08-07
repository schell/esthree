module Esthree.Print where

import           Aws.S3
import           Text.Printf
import           Data.List
import qualified Data.Text as T

printService :: GetServiceResponse -> IO ()
printService (GetServiceResponse user buckets) = do
    printUser user
    putStrLn "--"
    printBuckets buckets

printUser :: UserInfo -> IO ()
printUser (UserInfo uid name) =
    printCols ["User ID":[T.unpack uid], "User Name":[T.unpack name]]

printBucket :: GetBucketResponse -> IO ()
printBucket = undefined

printBuckets :: [BucketInfo] -> IO ()
printBuckets bs =
    let names = map (T.unpack . bucketName) bs
        dates = map (show . bucketCreationDate) bs
    in printCols ["Bucket":names, "Last Modified":dates]

printObjects :: [ObjectInfo] -> IO ()
printObjects os =
    let keys = map (T.unpack . objectKey) os
        mods = map (show . objectLastModified) os
        etgs = map (T.unpack . objectETag) os
        sizs = map (show . objectSize) os
        scls = map (show . objectStorageClass) os
    in printCols [keys, mods, etgs, sizs, scls]

printCols :: [[String]] -> IO ()
printCols strs =
    let strs' = map padCol strs
        strs'' = transpose strs'
        strs''' = map (intercalate "\t") strs''
    in mapM_ putStrLn strs'''

padCol :: [String] -> [String]
padCol ss = let n = foldl accLen 0 ss in map (pad n) ss

pad :: Int -> String -> String
pad n = printf ("%-" ++ show n ++ "s")

accLen :: Int -> String -> Int
accLen n s = if length s > n then length s else n
