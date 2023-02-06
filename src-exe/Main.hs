module Main where

import qualified Data.ByteString.Lazy as B

import System.Directory.Recursive (getFilesRecursive)
import System.Directory (makeAbsolute)
import Data.List (intercalate, groupBy, sortBy)
import Data.Function (on)
import Data.Digest.Pure.MD5 (md5)
import Control.Monad ((<$!>))

fileHash :: FilePath -> IO String
fileHash path =  show <$> (md5 <$!> B.readFile path)
 
getDuplicates :: FilePath -> IO [[FilePath]]
getDuplicates path = do 
        files <- mapM makeAbsolute =<< getFilesRecursive path
        hashes <- mapM fileHash files
        return $ map (map snd)
               $ filter ((>1) . length)
               $ groupBy ((==) `on` fst)
               $ sortBy (compare `on` fst) 
               $ zip hashes files

prettyPrinter :: [[String]] -> IO ()
prettyPrinter l = putStrLn output
        where 
        groups = map (\x -> ( intercalate "\n\n" x ) ++ "\n\n" ) l
        banner =  (take 20 $ repeat '-') ++ "\n"
        output  = banner ++ intercalate banner groups

main :: IO ()
main = getDuplicates "." >>= prettyPrinter 
