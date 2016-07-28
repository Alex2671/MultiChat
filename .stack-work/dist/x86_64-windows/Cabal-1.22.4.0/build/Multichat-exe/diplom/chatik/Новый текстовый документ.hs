module ServerApp

where
import Network.Socket hiding (recv,send)
import System.Environment
import Control.Concurrent(forkIO)
import Network.Socket.ByteString 
import qualified Data.ByteString.Char8 as B8
import Data.Word


data IncConnection =IncConnection  String Socket SockAddr
type TupleConnections = [IncConnection]


main :: IO ()
main = do 
	putStrLn "Chat starting..."
	server (fromInteger 3045)
	putStrLn "Socket closed"
-- binding socket for reading from port
server :: PortNumber -> IO ()
server port = withSocketsDo $ do 
				-- addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just $ show port)
				sock <- socket AF_INET Stream defaultProtocol
				-- let servaddr = head addrinfo
				-- sock <- socket (addrFamily servaddr) Stream defaultProtocol
			        setSocketOption sock ReuseAddr 1 
		                inet_addr "0" >>= bind sock . SockAddrInet 3045
				-- bind sock (SockAddrInet port iNADDR_ANY)
				-- bind sock (addrAddress servaddr)
				listen sock 4 
				sockHandler sock
				

--handle masseges and connections
sockHandler :: Socket -> IO ()
sockHandler sock = do 
		(sockh,addrs) <- accept sock
		forkIO $ putStrLn "Client Connected " >> receiveMessage (IncConnection "FirstDayn" sockh addrs)
		sockHandler sock

receiveMessage :: IncConnection -> IO ()
receiveMessage (IncConnection name sockh addrsockh) = 

-- receiveMessage :: Socket -> IO ()
-- receiveMessage sockh = do 
--		msg <- recv sockh 10
--		B8.putStrLn msg >> send sockh (B8.pack "sosi")
--		if msg == B8.pack "q" || B8.null msg
--			then sClose sockh >> putStrLn "Client Disc"
--			else receiveMessage sockh