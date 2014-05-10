module Main (main) where

import qualified Data.ByteString as B
import Data.Digest.Pure.Adler32
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  inp <- B.getContents
  let buffers = take (read n) $ B.tails inp
  print $ map adler32 buffers
