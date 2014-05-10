module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  inp <- B.getContents
  let buffers = take (read n) $ B.tails inp
  print $ map sha1 buffers
