{-# LANGUAGE Rank2Types #-}
-- | A collection of utility functions for dealing with 'Enumerator's.
module Network.Wai.Enumerator
    ( -- * Utilities
      mapE
      -- * Conversions
    , -- ** Lazy byte strings
      toLBS
    , fromLBS
    , fromLBS'
      -- ** Source
    , toSource
      -- ** Handle
    , fromHandle
    , fromHandle'
      -- ** FilePath
    , fromFile
    , fromFile'
    , fromTempFile
    , fromResponseBody
    ) where

import Network.Wai (Enumerator (..), Source (..), ResponseBody (..))
import qualified Network.Wai.Source as Source
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO (withBinaryFile, IOMode (ReadMode), Handle, hIsEOF, hClose)
import System.Directory (removeFile)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad ((<=<))

-- | Performs a specified conversion on each 'B.ByteString' output by an
-- enumerator.
mapE :: (B.ByteString -> B.ByteString) -> Enumerator -> Enumerator
mapE f (Enumerator e) = Enumerator $ \iter -> e (iter' iter) where
    iter' iter a = iter a . f

-- | This uses 'unsafeInterleaveIO' to lazily read from an enumerator. All
-- normal lazy I/O warnings apply. In addition, since it is based on
-- 'toSource', please observe all precautions for that function.
toLBS :: Enumerator -> IO L.ByteString
toLBS = Source.toLBS <=< toSource

-- | This function safely converts a lazy bytestring into an enumerator.
fromLBS :: L.ByteString -> Enumerator
fromLBS lbs = Enumerator $ \iter a0 -> helper iter a0 $ L.toChunks lbs where
    helper _ a [] = return $ Right a
    helper iter a (x:xs) = do
        ea <- iter a x
        case ea of
            Left a' -> return $ Left a'
            Right a' -> helper iter a' xs

-- | Same as 'fromLBS', but the lazy bytestring is in the IO monad. This allows
-- you to lazily read a file into memory, perform some mapping on the data and
-- convert it into an enumerator.
fromLBS' :: IO L.ByteString -> Enumerator
fromLBS' lbs' = Enumerator $ \iter a0 -> lbs' >>= \lbs ->
    runEnumerator (fromLBS lbs) iter a0

-- | This function uses another thread to convert an 'Enumerator' to a
-- 'Source'. In essence, this allows you to write code which \"pulls\" instead
-- of code which is pushed to. While this can be a useful technique, some
-- caveats apply:
--
-- * It will be more resource heavy than using the 'Enumerator' directly.
--
-- * You *must* consume all input. If you do not, then the other thread will be
-- deadlocked.
toSource :: Enumerator -> IO Source
toSource (Enumerator e) = do
    buff <- newEmptyMVar
    _ <- forkIO $ e (helper buff) () >> putMVar buff Nothing
    return $ source buff
      where
        helper :: MVar (Maybe B.ByteString)
               -> ()
               -> B.ByteString
               -> IO (Either () ())
        helper buff _ bs = do
            putMVar buff $ Just bs
            return $ Right ()
        source :: MVar (Maybe B.ByteString)
               -> Source
        source mmbs = Source $ do
            mbs <- takeMVar mmbs
            case mbs of
                Nothing -> do
                    -- By putting Nothing back in, the source can be called
                    -- again without causing a deadlock.
                    putMVar mmbs Nothing
                    return Nothing
                Just bs -> return $ Just (bs, source mmbs)

-- | Read a chunk of data from the given 'Handle' at a time. We use
-- 'defaultChunkSize' from the bytestring package to determine the largest
-- chunk to take.
fromHandle' :: Handle -> IO a -> Enumerator
fromHandle' h onEOF = go
 where
  go = Enumerator $ \iter a -> do
    eof <- hIsEOF h
    if eof
        then do
            _ <- onEOF
            return $ Right a
        else do
            bs <- B.hGet h defaultChunkSize
            ea' <- iter a bs
            case ea' of
                Left a' -> return $ Left a'
                Right a' -> runEnumerator go iter a'

-- | Read from a handle and close after EOF
fromHandle :: Handle -> Enumerator
fromHandle h = fromHandle' h $ hClose h

-- | A little wrapper around fromHandle' which first opens a file for reading.
-- The handle is closed before the onEOF action is performed
fromFile' :: FilePath -> IO a -> Enumerator
fromFile' fp onEOF = Enumerator $ \iter a0 -> withBinaryFile fp ReadMode $ \h ->
    runEnumerator (fromHandle' h $ hClose h >> onEOF) iter a0

-- | Enumerator to read and close a file
fromFile :: FilePath -> Enumerator
fromFile fp = fromFile' fp $ return ()

-- | Enumerator to read and remove a file
fromTempFile :: FilePath -> Enumerator
fromTempFile fp = fromFile' fp $ removeFile fp

-- | Since the response body is defined as a 'ResponseBody', this function
-- simply reduces the whole value to an enumerator. This can be convenient for
-- server implementations not optimizing file sending.
fromResponseBody :: ResponseBody -> Enumerator
fromResponseBody (ResponseEnumerator e) = e
fromResponseBody (ResponseLBS lbs) = fromLBS lbs
fromResponseBody (ResponseFile fp) = fromFile fp
