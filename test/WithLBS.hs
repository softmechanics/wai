module WithLBS where

import Network.Wai.Util
import Test.HUnit
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as E
import Blaze.ByteString.Builder
import Data.Monoid

src  = map B8.pack [show i | i <- [0 .. 10]]
out  = L.fromChunks src

withLBSTester :: (L.ByteString -> IO L.ByteString) -> IO L.ByteString
withLBSTester f = do
  let enum = E.enumList 2 src
  bs <- E.run_ (enum $$ E.joinI $ withLBS f $$ EL.consume)
  let bs' = map toByteString bs
  return $ L.fromChunks bs'

fromRight def (Left _) = def
fromRight _ (Right r) = r

testWithLBS = test [
    "withLBS complete" ~: do
      res <- withLBSTester (\bs -> print bs >> return bs)
      print $ (L8.unpack res, L8.unpack out)
      assert $ res == out
  , "withLBS parital" ~: do
      res <- withLBSTester (return . L.fromChunks . take 1 . L.toChunks) :: IO L.ByteString
      print $ (L8.unpack res, "0")
      assert $ L8.unpack res == "0"
  ]

