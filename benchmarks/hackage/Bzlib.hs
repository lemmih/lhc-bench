module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.BZip as BZip
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  inp <- B.getContents
  replicateM_ (read n) (roundTrip inp)

roundTrip inp = do
    evaluate (inp == process inp)
  where
    process = BZip.compress . BZip.decompress

