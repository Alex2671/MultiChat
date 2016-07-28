module ClientChat
where 

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)

main :: IO ()
main = do
  
  client "localhost" 3034

client :: String -> Int -> IO ()
client host port = do
			addressInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
			sock <- socket (addrAddress serverAddr) Stream defaultProtocol
			connect sock (addrAddress serverAddr)
			msgSender sock
                sClose sock
    where serverAddr = head addrInfo

msgSender :: Socket -> IO ()
msgSender sock = do
	msg <- B8.getLine
	send sock msg
	rMsg <- recv sock 10
	B8.putStrLn rMsg
	if msg == B8.pack "q" then putStrLn "Disconnected..." else msgSender sock                