module Lib 
    where
import System.Console.ANSI
import System.IO
import System.Environment
import Data.Text (split,unpack,pack,toLower,words)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Network (listenOn)
import Control.Concurrent (forkIO,killThread)
import Data.Word
import Control.Concurrent.Chan
import Data.List


data Commandhandle = Bool | Commandhandle | String

setConsole :: IO ()
setConsole  = do
			setTitle "Chat Service by Alexey Grachkov"
			setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid White,SetVisible True]
			setSGR [SetConsoleIntensity FaintIntensity,SetColor Background Dull Yellow]
			return ()
setChatParams :: Color -> Color -> IO ()
setChatParams back font = do
			setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid back,SetVisible True]
			setSGR [SetConsoleIntensity FaintIntensity,SetColor Background Dull font]

commandController :: String -> IO String
commandController nick = do 
				commands <- getLine 
				sequre commands 
	 where sequre word 
	 			| firsArg == "" = putStrLn "Empty List!Try again!" >> commandController nick
	  			| firsArg == "connect" = client "0" nick 3045 >> return "Connected"
				| firsArg == "host" = server (fromIntegral 3045) 4 (B8.pack nick) >> return "poka!"
			 	| firsArg == "q" = return "Exiting..."
			 	| firsArg == "settings" = return "To change words - words [color] \nTo change background - back [color]" >> commandController nick
				| firsArg == "!list" = putStrLn "Connect [hostname] [port] - connect to channel \nHost [port] - start your channel \nq - exiting" >> commandController nick
				| otherwise = putStrLn "Wrong command.Try again!" >> commandController nick
				where firsArg = unpack $ toLower $ head $ Data.Text.words $ pack word


--client functions
client :: String -> String -> PortNumber -> IO ()	
client host nick port = withSocketsDo $ do 
			addrs <- getAddrInfo (Just hints) (Just "192.168.1.4") (Just "3045") 
			-- addrs <- getAddrInfo Nothing Nothing (Just $ show port) 
			sock <- socket AF_INET Stream defaultProtocol
			fmap (\inetAdders -> bind sock $ SockAddrInet 3045 inetAdders) $ inet_addr "111111" 
			setSocketOption sock ReuseAddr 1
			connect sock (addrAddress $ head addrs)
			msgSender sock nick
			
			
		where 

			  hints = defaultHints{addrFlags = [AI_ADDRCONFIG,AI_CANONNAME]}
msgSender :: Socket -> String -> IO ()
msgSender sock nick = do
	msg <- B8.getLine
	send sock $ B8.pack nick
	send sock msg
	forkIO receiveMes
	if msg == B8.pack "q" then putStrLn "Disconnected..." else msgSender sock nick 			
  where 
  	receiveMes :: IO ()
  	receiveMes = do 
				rNick <- recv sock 50
				rMsg <- recv sock 50
				setChatParams White Green
				B8.putStrLn rNick
				setChatParams White Blue
				B8.putStrLn rMsg				
				receiveMes



-- host dates
type Clienti = (B8.ByteString,Socket,SockAddr)

--host functions
--binding socket for read from the port
server :: PortNumber -> Int -> B8.ByteString -> IO ()
server port people nick = withSocketsDo $ do 
				addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just $ show port)
				let servaddr = head addrinfo 
				sock <- socket (addrFamily servaddr) Stream defaultProtocol
				setSocketOption sock ReuseAddr 1 
				bind sock (addrAddress servaddr)
				listen sock people 
				putStrLn "Channel was created"
				mainChany <- newChan
				sockHandler sock nick [] mainChany


--handle masseges
sockHandler :: Socket -> B8.ByteString -> [Clienti] -> Chan (B8.ByteString,B8.ByteString) -> IO ()
sockHandler sock nick clients mainChany = do 
		idHandle <- forkIO $ massSpam mainChany clients 
		(sockh,port) <- accept sock
		duppedChan <- dupChan mainChany
		takeNickClient <- BS.recv sockh 50
		writeChan mainChany (takeNickClient,B8.append takeNickClient $ B8.pack "Connected" )
		idRes <- forkIO $ setConsole >> putStrLn "Client Connected" >> receiveMessage sockh takeNickClient duppedChan
		killThread idHandle
		sockHandler sock nick (clients ++ [(takeNickClient,sockh,port)])  mainChany
	where 	
		massSpam :: Chan (B8.ByteString,B8.ByteString) -> [Clienti] -> IO ()
		massSpam mainchan [] = return ()
		massSpam chan clients = do
							(author,text) <- readChan chan
						 	mapM_ (\(nick,socket,port) -> send socket nick >> send socket (B8.append text author) ) (clients :: [(B8.ByteString,Socket,SockAddr)]) 

receiveMessage :: Socket -> B8.ByteString -> Chan (B8.ByteString,B8.ByteString) -> IO ()
receiveMessage sockh nick duppedChan = do 
		msg <- BS.recv sockh 50
		setChatParams White Green
		B8.putStrLn nick
		setChatParams White Blue
		B8.putStrLn msg
		writeChan duppedChan (nick,msg)
		if msg == B8.pack "q" || B8.null msg
			then sClose sockh >> putStrLn "Client Disc"
			else receiveMessage sockh nick duppedChan
		