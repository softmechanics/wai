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
app = AppEnum $ \req f ->
  case pathInfo req of
    {--
    "/post/" -> do
        bss <- consume
        postResponse $ L.fromChunks bss
    --}
    "/postWithLBSComplete/" -> do
        joinI $ withLBS return $$ f status200 [("Content-Type", "text/plain"), ("Transfer-Encoding", "chunked")]
    "/postWithLBSPartial/" -> do
        joinI $ withLBS (return . L.fromChunks . take 1 . L.toChunks) $$ f status200 [("Content-Type", "text/plain")]

    {--
    "/postWithLBSPartial/" -> do
        eiter <- withLBS (postResponse . L.fromChunks . take 1 . L.toChunks)
        case eiter of
             Left e -> liftIO $ throwIO e
             Right i -> return i
    _ -> indexResponse
    --}

indexResponse :: Iteratee ByteString IO Response
indexResponse = return $ ResponseFile
    status200
    [("Content-Type" , "text/html")]
    "index.html"

postResponse :: Monad m => L.ByteString -> m Response
postResponse lbs = return $ ResponseBuilder
    status200
    [("Content-Type", "text/plain")]
    b 
  where !b = (fromLazyByteString lbs)
