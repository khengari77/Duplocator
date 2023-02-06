module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import System.IO
import System.Directory.Recursive (getFilesRecursive)
import System.Directory (makeAbsolute)
import Data.List (intercalate)
import Data.Digest.Pure.MD5 (md5)
import Control.Monad (liftM)

data File = File { path :: FilePath, hash :: String}
          deriving (Show)


fileRead :: FilePath -> IO File
fileRead path = do
                fullPath <- makeAbsolute path
                hash <- liftM (show . md5) . B.readFile $ path
                return (File fullPath  hash)
 
readDirectoryFiles :: FilePath -> IO [File]
readDirectoryFiles path = mapM fileRead =<< getFilesRecursive path

fileHashMap :: [File] -> M.Map String [FilePath]
fileHashMap [] = M.empty
fileHashMap (file:files) = M.insertWith (++) key value map
            where
            key = hash file
            value = [path file]
            map =  fileHashMap files  

getDuplicates :: FilePath -> IO [[FilePath]]
getDuplicates directory = do 
                     out <- fmap M.elems duplicates
                     return out
                     where
                     files = readDirectoryFiles directory
                     map = fmap fileHashMap files
                     duplicates = fmap (M.filter ((>1) . length)) map

prettyPrinter :: [[String]] -> IO ()
prettyPrinter l = putStrLn output
                where 
                groups = map (\x -> ( intercalate "\n\n" x ) ++ "\n\n" ) l
                banner =  (take 20 $ repeat '-') ++ "\n"
                output  = banner ++ intercalate banner groups
main :: IO ()
main = getDuplicates "." >>= prettyPrinter 
