module Main where

import Control.Monad
import qualified Codec.Archive.Tar as Tar
import Language.Haskell.Exts
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
  inp <- B.getContents
  let files = unpack (Tar.read inp)
  forM_ files $ \(path, file) -> do
     case parseFileContentsWithMode pMode (T.unpack file) of
      ParseOk{}     -> return ()
      ParseFailed loc msg -> error $ "ParseFailed: " ++ msg ++ "\n"++show loc

pMode = defaultParseMode{ fixities = Nothing, extensions = exts }
exts = [ EnableExtension FlexibleContexts
       , EnableExtension CPP
       , EnableExtension TypeOperators
       , EnableExtension MagicHash ]

unpack entries =
  case entries of
    Tar.Next entry next ->
      case Tar.entryContent entry of
        Tar.NormalFile bs _ -> (Tar.entryPath entry, decodeUtf8 bs) : unpack next
        _ -> unpack next
    Tar.Done -> []
    Tar.Fail e -> error (show e)
