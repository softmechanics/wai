module Network.Wai.Util (
    withLBS
  --, withLBS_
  ) where

import Control.Concurrent
import Control.Exception (SomeException, throwIO, catch, finally)
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Enumerator ((>>==), ($$))
import qualified Data.Enumerator as E
import System.IO.Unsafe
import Prelude hiding (catch)
import Blaze.ByteString.Builder
import Network.Wai

type MMRes a = MVar (Maybe (Either SomeException a)) 

{--
withLBS_ :: (L.ByteString -> IO a) -> E.Iteratee B.ByteString IO a
withLBS_ f = do
  e <- withLBS f
  case e of
       Left x -> liftIO $ throwIO x
       Right a -> return a
--}
{--
putMVarWhen mNeedMore mout [] = do
  needMore <- takeMVar mNeedMore
  if needMore 
     then putMVar mout []
     else return ()
putMVarWhen mNeedMore mout (x:xs) = do
  needMore <- takeMVar mNeedMore
  if needMore 
     then putMVar mout x >> putMVarWehn mNeedMore mout xs
     else return ()
--}

withLBS :: (L.ByteString -> IO (Status, ResponseHeaders, L.ByteString)) -> ResponseEnumeratee a
withLBS lbsApp responseIter = do
  min <- liftIO $ newEmptyMVar
  mout <- liftIO $ newEmptyMVar
  mNeedMoreOut <- liftIO $ newEmptyMVar 
  miter <- liftIO $ newEmptyMVar

  _ <- liftIO $ forkIO $ do
           let loop [] = do
                   putStrLn "end of output"
                   putMVar mout $ Left Nothing
               loop (x:xs) = do
                   putStrLn "taking need more out"
                   b <- takeMVar mNeedMoreOut
                   putStrLn $ "need more out: " ++ show b
                   if b
                      then do putStrLn $ "sending out: " ++ show x
                              putMVar mout $ Right x
                              loop xs
                      else return ()
           catch 
             (do (status, hdrs, lbs) <- lbsApp =<< evalLBS min
                 putMVar miter $ responseIter status hdrs
                 loop $ L.toChunks lbs)
             (putMVar mout . Left . Just)

  iter <- liftIO $ takeMVar miter
  E.joinI $ enumerateeLBS mNeedMoreOut min mout $$ iter

evalLBS :: MVar [B.ByteString] -> IO L.ByteString
evalLBS mbs = fmap L.fromChunks go
  where go = unsafeInterleaveIO $ do 
                putStrLn "taking"
                next <- takeMVar mbs
                putStrLn $ "took " ++ show next
                case next of
                     [] -> return []
                     bs -> do fmap (bs ++) go

-- try to read from mout until mNeedMoreIn is true
enumerateeLBS :: MVar Bool
              -> MVar [B.ByteString]
              -> MVar (Either (Maybe SomeException) B.ByteString)
              -> E.Enumeratee B.ByteString Builder IO a
enumerateeLBS mNeedMoreOut min mout = checkDone (E.continue . step) 
  where step :: (E.Stream Builder -> E.Iteratee Builder IO a)
             -> E.Stream B.ByteString 
             -> E.Iteratee B.ByteString IO (E.Step Builder IO a)
        step k E.EOF = do
          -- signal no more input to evalLBS.  This could block while we drain, so put from another thread
          liftIO $ putStrLn "put EOF"
          liftIO $ forkIO $ putMVar min [] 
          a <- liftIO $ E.run_ $ drain $$ E.continue k
          E.yield (E.Yield a E.EOF) E.EOF
        step k (E.Chunks []) = E.continue $ step k
        step k (E.Chunks xs) = loop k xs

        drain :: E.Enumerator Builder IO a
        drain = E.generateM doDrain 

        doDrain :: IO (Maybe Builder)
        doDrain = do
          -- drain output
          liftIO $ putStrLn "draining mout"
          liftIO $ tryPutMVar mNeedMoreOut True 
          out <- liftIO $ takeMVar mout
          liftIO $ putStrLn $ "got mout: " ++ show out
          case out of
               Right r -> do
                 putStrLn $ "drained: " ++ show r
                 return $ Just $ fromByteString r
               Left Nothing -> return Nothing
               Left (Just e) -> throwIO e
                 

        checkDone :: ((E.Stream Builder -> E.Iteratee Builder IO a) -> E.Iteratee B.ByteString IO (E.Step Builder IO a))
                  -> E.Enumeratee B.ByteString Builder IO a
        checkDone f (E.Continue k) = do
          liftIO $ tryPutMVar mNeedMoreOut True 
          f k
        checkDone _ step = do
          liftIO $ putStr "dont need more out"
          liftIO $ putMVar mNeedMoreOut False 
          liftIO $ putStrLn "."
          E.yield step (E.Chunks [])

        loop :: (E.Stream Builder -> E.Iteratee Builder IO a) -> [B.ByteString] -> E.Iteratee B.ByteString IO (E.Step Builder IO a)
        loop k xs = do
          -- try to read output
          liftIO $ putStr "checking needmore: "
          put <- liftIO $ tryPutMVar min xs
          liftIO $ print put
          if put
             then E.continue (step k)
             else do
               liftIO $ putStrLn "trying mout"
               out <- liftIO $ tryTakeMVar mout
               liftIO $ putStrLn $ "got mout: " ++ show out
               case out of
                    Just (Right x) -> do
                      liftIO $ tryPutMVar mNeedMoreOut True 
                      liftIO $ print x
                      k (E.Chunks [fromByteString x]) >>== checkDone (E.continue . step)
                    Just (Left Nothing) -> do
                      -- eof
                      liftIO $ putStrLn "no more output"
                      k E.EOF >>== E.checkDone (E.continue . step)
                    Just (Left (Just e)) -> do
                      liftIO $ putStrLn "exception!"
                      liftIO $ throwIO e
                    Nothing -> do 
                      -- empty
                      liftIO $ tryPutMVar mNeedMoreOut True 
                      liftIO yield
                      loop k xs

{--
        loop k xs = do
          liftIO $ putStrLn $ "putting min: " ++ show xs
          liftIO $ putMVar min xs
          liftIO $ putStrLn $ "put min"
          loop k []
--}


{--
iterateLBS :: MVar [B.ByteString] 
           -> MVar B.ByteString
           -> E.Iteratee B.ByteString IO (Either SomeException a)
iterateLBS min mout mbs = E.continue go
  where go (E.Chunks []) = E.continue go

        go (E.Chunks cs) = do 
          mres <- liftIO $ waitPut cs
          case mres of
               Nothing    -> do liftIO $ putStrLn "continuing"
                                E.continue go
               (Just res) -> do liftIO $ putStrLn "stopping"
                                E.yield res $ E.Chunks cs

        go E.EOF = do 
          -- TODO check for nothing, and report error
          Just mres <- liftIO $ waitPut []
          E.yield mres E.EOF

        waitPut cs = do
          putStrLn "waiting"
          mres <- takeMVar mmres
          putStrLn "done waiting"
          case mres of
               Nothing  -> putMVar mbs cs
               _        -> return ()
          return mres

--}
