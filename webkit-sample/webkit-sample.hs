{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Network.Wai
import Network.Wai.Util
import Network.Wai.Handler.Warp
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (consume, Iteratee, run_, ($$), joinI)
import Blaze.ByteString.Builder (fromLazyByteString)

main :: IO ()
main = putStrLn "http://localhost:3000/" >> run 3000 app

app :: Application
app = AppEnum $ \req ->
  case pathInfo req of
    "/postWithLBSComplete/" -> 
        withLBS $ \lbs -> return (
            status200
          , [("Content-Type", "text/plain"), ("Transfer-Encoding", "chunked")]
          , lbs
          )
    "/postWithLBSPartial/" -> 
        withLBS $ \lbs -> return (
            status200
          , [("Content-Type", "text/plain")]
          , L.fromChunks $ take 1 $ L.toChunks lbs
          )

