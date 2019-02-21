module Server (
    runServer,
    ) where

import Control.Exception.Safe
import Network.Socket hiding (recv) 
import Network.Socket.ByteString
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString as BS

runServer :: String -> IO ()
runServer port = do
    addr <- resolve port
    bracket (open addr) close svMain

svMain :: Socket -> IO ()
svMain sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    void $ forkFinally (talk conn) $ \_ -> close conn

talk :: Socket -> IO () 
talk conn = do
    msg <- recv conn 1024
    unless (BS.null msg) $ do
        sendAll conn msg
        talk conn
    
resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [ AI_PASSIVE ]
                             , addrSocketType = Stream
                             }
    head <$> getAddrInfo (Just hints) Nothing (Just port)
    -- getAddrInfo does not return an empty list

open :: AddrInfo -> IO Socket
open addr = do
    sock :: Socket <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    setCloseOnExecIfNeeded (fdSocket sock)
    bind sock (addrAddress addr)
    listen sock 10
    pure sock
