module Client (
    runClient,
    ) where

import Control.Exception.Safe
import qualified Data.ByteString.Char8 as BC
import Network.Socket hiding (recv)
import Network.Socket.ByteString

runClient :: String -> String -> IO ()
runClient host port = withSocketsDo $ do
    addr <- resolve host port
    bracket (open addr) close talk

resolve :: String -> String -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)
    connect sock (addrAddress addr)
    pure sock

talk :: Socket -> IO ()
talk sock = do
    sendAll sock "Hello fren"
    msg <- recv sock 1024
    BC.putStrLn $ "Received: " <> msg
