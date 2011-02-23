module Network.Wai.Util (
    withLBS
  ) where

import Control.Concurrent
import Control.Exception (SomeException, catch, finally)
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Enumerator as E
import System.IO.Unsafe
import Prelude hiding (catch)

withLBS :: (L.ByteString -> IO a) -> E.Iteratee B.ByteString IO (Either SomeException a)
withLBS f = do
  (mcont, mbs, mres) <- liftIO $ do
    mbs <- newEmptyMVar
    mres <- newEmptyMVar
    mcont <- newMVar True
    _ <- forkIO $ finally 
      (catch
        (do a <- f =<< evalLBS mcont mbs
            a `seq` putMVar mres (Right a)) 
        (putMVar mres . Left))
      (putMVar mcont False)
    return (mcont, mbs, mres)
  iterateLBS mcont mbs
  liftIO $ takeMVar mres

evalLBS :: MVar Bool -> MVar [B.ByteString] -> IO L.ByteString
evalLBS mcont mbs = fmap L.fromChunks go
  where go = unsafeInterleaveIO $ do 
                next <- takeMVar mbs
                case next of
                     [] -> return []
                     bs -> do putMVar mcont True
                              fmap (bs ++) go

iterateLBS :: MVar Bool -> MVar [B.ByteString] -> E.Iteratee B.ByteString IO ()
iterateLBS mcont mbs = E.continue go
  where go (E.Chunks []) = E.continue go

        go (E.Chunks cs) = do 
          cont <- liftIO $ waitPut cs
          if cont
             then E.continue go
             else E.yield () $ E.Chunks cs

        go E.EOF = do 
          _ <- liftIO $ waitPut []
          E.yield () E.EOF

        waitPut cs = do
          cont <- takeMVar mcont
          when cont $ putMVar mbs cs
          return cont

