 {-# LANGUAGE OverloadedStrings #-}   	

module Lib 
    where   	
import System.Console.ANSI
import System.IO
import System.Environment
import Data.Text (split,unpack,pack,toLower,words,Text)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent (forkIO,killThread,ThreadId,threadDelay,yield)
import Data.Word
import Control.Concurrent.Chan
import Data.List
import Network.Wai.Handler.Launch
import Network.Wai	
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Blaze.ByteString.Builder
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import qualified Network.WebSockets as WS 


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

-- controllers chat service <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

--controller in start of the programm
commandController :: String -> IO String
commandController nick = do 
				commands <- getLine
				sequre commands 
	 where sequre word 
	 			| firsArg == "" = putStrLn "Empty List!Try again!" >> commandController nick
	  			| firsArg == "!connect" = client "192.168.1.129" nick (fromIntegral 3045)  >> return "Connected"
				| firsArg == "!host" = do
					putStrLn "RTCMEDIA started"
					state <- newMVar emptyServerState
					idServerThread <- forkIO $ WS.runServer "192.168.1.129" 4001 $ application state
					server (fromIntegral 3055) 4 (B8.pack nick) >> return "poka!"
			 	| firsArg == "!q" = return "Exiting..."
			 	| firsArg == "!settings" = return "To change words - words [color] \nTo change background - back [color]" >> commandController nick
				| firsArg == "!list" = putStrLn "!Connect [hostname] [port] - connect to channel \n!Host [port] - start your channel \n!q - exiting" >> commandController nick
				| otherwise = putStrLn "Wrong command.Try again!" >> commandController nick
				where firsArg = unpack $ toLower $ head $ Data.Text.words $ pack $ word

--client functions
client :: String -> String -> PortNumber -> IO ()	
client host nick port = withSocketsDo $ do
			addrs <- getAddrInfo (Just hints) (Just host) (Just "3055") 
			---- addrs <- getAddrInfo Nothing Nothing (Just $ show port) 
			sock <- socket AF_INET Stream defaultProtocol
			fmap (\inetAdders -> bind sock $ SockAddrInet port inetAdders) $ inet_addr "11111" 
			setSocketOption sock ReuseAddr 1
			connect sock (addrAddress $ head addrs)
			mes <- BS.recv sock 150
			B8.putStrLn mes
			send sock $ B8.pack nick
			msgSender sock nick 
						
		where 

			  hints = defaultHints{addrFlags = [AI_ADDRCONFIG,AI_CANONNAME]}
msgSender :: Socket -> String -> IO ()
msgSender sock nick  = do
			idThr <- forkIO receiveMes >>= return . Just
			chatController sock Nothing
 where 
  	receiveMes :: IO ()
  	receiveMes  = do 
  				rNick <- recv sock 150
				rMsg <- recv sock 150
				B8.putStrLn ""
				setChatParams White Green
				B8.putStrLn rNick
				setChatParams White Blue
				B8.putStrLn rMsg				
				receiveMes
	chatController :: Socket -> Maybe ThreadId -> IO ()
	chatController socke thread = do
					commands <- getLine
					sequre  commands 
					
	 where 
	 	sequre :: String -> IO ()
	 	sequre word = case word of 
				"!video" ->  do
										idVideo <- if thread == Nothing then do 
																			thr <- forkIO $ openVideo "GrachkovChat" 4005
																			setConsole		
																			putStrLn "Video stream started"
																			return $ Just thr  
											 	   else return thread
									    --didVideo <- if thread == Nothing then do 
													--						thr <- forkIO $ openVideo "GrachkovChat" 4001
													--						return $ Just thr  
											 	--   else return thread
										--sock <- socket (addrFamily serverAddr) Stream defaultProtocol
										--bindSocket sock (addrAddress serverAddr)
										
										--addreses <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4000")
										
										--bond <- isListening  sock
										--soskAddr <- getSocketName sock
										--sockPor <- isReadable  sock
										--putStrLn $ show bond
										--putStrLn $ show soskAddr
										--putStrLn $ show sockPor
										--putStrLn $ show as
										--putStrLn $ show aas
										chatController socke idVideo 
			 	"!disconnect" -> putStrLn "Exiting" >> return ()
				"!list" -> putStrLn "!video  - translate video \n!disconnect - exit from the channel" >> chatController socke thread
				_ -> do
							 BS.send socke $ B8.pack word 	
							 chatController socke thread	
		

-- host dates
type Clienti = (B8.ByteString,Socket,SockAddr)

--host functions
--binding socket for read from the port
server :: PortNumber -> Int -> B8.ByteString -> IO ()
server port people nick = withSocketsDo $ do 
				mainChany <- newChan
				--addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just $ show port)
				--let servaddr = head addrinfo 
				--sock <- socket (addrFamily servaddr) Stream defaultProtocol
				--sdf <- inet_addr "341515"
				--putStrLn $ show $ isSupportedSockAddr $ SockAddrInet 3055 sdf
				sock <- socket AF_INET Stream defaultProtocol
				setSocketOption sock ReuseAddr 1 
				inet_addr "0" >>= bind sock . SockAddrInet port
				listen sock people 
				bond <- isListening  sock
				soskAddr <- getSocketName sock
				sockPor <- isReadable  sock
				putStrLn $ show bond
				putStrLn $ show soskAddr
				putStrLn $ show sockPor
				(as,aas) <- inet_addr "0" >>= getNameInfo [] True True . SockAddrInet port
				putStrLn $ show as
				putStrLn $ show aas
				putStrLn "Channel created..."
				mvr <- newEmptyMVar
				mvrcountCl <- newEmptyMVar
				sockHandler sock nick mvrcountCl mainChany mvr  Nothing Nothing Nothing

--hdl <- socketToHandle sockh ReadWriteMode
		--hSetBuffering hdl LineBuffering
		--hSetEncoding hdl utf8
		--hPutStrLn hdl "Hello!"
--handle massegess
sockHandler :: Socket -> B8.ByteString -> MVar [Clienti]  -> Chan (B8.ByteString,B8.ByteString) -> MVar Clienti -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId -> IO ()
sockHandler sock nick clients mainChany mvr handlethread thread clientThread = withSocketsDo $ do 
		idClientHandle <-  if clientThread == Nothing then do
											idWor <- forkIO $ clientCon mvr clients 
											putStrLn "clientCon started"
											return $ Just idWor
						   else return clientThread		
		idHandle <- if handlethread == Nothing then do
												idsh <- forkIO $ massSpam mainChany clients [] 
												putStrLn "massSpam started"
												return $ Just idsh
					else return handlethread
		(sockh,address) <- accept sock
		BS.send sockh "HELLO"
		takeNickClient <- BS.recv sockh 150
		duppedChan <- dupChan mainChany
		putMVar mvr (takeNickClient,sockh,address)
		idWork <- if thread == Nothing then do
							idWor <- forkIO $ client "192.168.1.129" (B8.unpack nick) (fromIntegral 3066)
							return $ Just idWor
					 else return thread		
		idRes <- forkIO $   receiveMessage sockh takeNickClient duppedChan
		sockHandler sock nick clients mainChany mvr idHandle idWork idClientHandle
	where 	
		massSpam :: Chan (B8.ByteString,B8.ByteString) ->  MVar [Clienti] -> [Clienti]  ->  IO ()
		massSpam chan clients asd@[] = do 
									evenOneClient <- takeMVar clients
									massSpam chan clients (asd ++ evenOneClient)
		massSpam chan clients pe  = do
								newClient <- tryTakeMVar clients	
								if newClient == Nothing then do 	
												(author,text) <- readChan chan
												mapM_ (\(nick,socket,address) -> BS.send socket author >> BS.send socket text) pe
								 				massSpam chan clients pe
								else do
									Just list <- return $ (return (\x -> pe ++ x)) <*>  newClient  
									(author,text) <- readChan chan
									mapM_ (\(nick,socket,address) -> BS.send socket author >> BS.send socket text) list 
									massSpam chan clients list  
		clientCon :: MVar Clienti  -> MVar [Clienti] -> IO ()
		clientCon mvr clients = do
							connectedClient <- takeMVar mvr
							setConsole
							putStrLn "Client connected" 
 							trytakeClients <- tryTakeMVar clients
 							sd <- if trytakeClients == Nothing then putMVar clients [connectedClient] >> return Nothing 
 							else return $ fmap (\allConnected -> putMVar clients (allConnected ++ [connectedClient]) >> return Nothing) trytakeClients
			  				clientCon mvr clients 	 	

--(B8.append text author)
receiveMessage :: Socket -> B8.ByteString -> Chan (B8.ByteString,B8.ByteString) -> IO ()
receiveMessage sockh nick duppedChan = do 
		msg <- BS.recv sockh 150
		writeChan duppedChan (nick,msg)
		receiveMessage sockh nick duppedChan


--------------------------------------------------------------------------------------------------------------------------
-- server videocon API

type Client = (Text,WS.Connection)
type ServerState = [Client]
--parser nick into json
data Nickname = Nickname Text deriving (Show, Eq)
instance ToJSON Nickname where
	toJSON (Nickname nick) = object ["nickname" .= nick]
instance FromJSON Nickname where
	 	parseJSON (Object n) = Nickname <$> n .: "nickname"
	 	parseJSON _ = mzero
-- define list to sent the user
data UserList = UserList [Text] deriving (Show,Eq)
instance ToJSON UserList where
	 	toJSON (UserList js) = object ["userlist" .= js]
instance FromJSON UserList where
	 	parseJSON (Object js) = UserList <$> js .: "userlist"  	 	 	 	 
	 	parseJSON _ = mzero
-- data wrapper for SDP
data SDP = SDP 
			{
				sdp :: Text
			,   target :: Text
			} deriving (Show, Eq)
instance ToJSON SDP where
		toJSON (SDP s t) = object ["sdp" .= s, "target" .= t]
instance FromJSON SDP where
		parseJSON (Object o) = SDP <$> o .: "sdp" <*> o .: "target"
		parseJSON _ = mzero 		 				 	
-------------------------------------------------------------------
-- easyhand func --
emptyServerState :: ServerState
emptyServerState = []
--
numUsers :: ServerState -> Int
numUsers = length
--
isUserConnected :: Client -> ServerState -> Bool
isUserConnected client = any ((== fst client) . fst)
--
getConnection :: Text -> ServerState -> Maybe WS.Connection
getConnection _ [] = Nothing
getConnection nick (x:xs) | nick == fst x = Just (snd x)
						  |	otherwise = getConnection nick xs
--
addUser :: Client -> ServerState -> Either ServerState ServerState
addUser client state | isUserConnected client state = Left state
					 | otherwise = Right $ client : state
--
removeUser :: Client -> ServerState -> ServerState
removeUser client = filter (( /= fst client) . fst)
---------------------------------------------------------------------------

application :: MVar ServerState -> WS.ServerApp
application state pending = do
		conn <- WS.acceptRequest pending
		users <- liftIO $ readMVar state
		putStrLn "New Connected"
		msg <- WS.receiveData conn
		case decode msg of 
			Just (Nickname nick) -> flip finally (disconnect (nick,conn)) $ do
				liftIO $ modifyMVar_ state $ \s -> do
					case addUser (nick, conn) s of 
						Right newState -> do
							setConsole
							putStrLn $ "New user ready.Total count:" ++ (show $ numUsers newState)
							return newState
						Left oldState -> do
							setConsole
							putStrLn $ "User already conencted!"
							return oldState
				handleUser conn state nick 
			_ -> do
				putStrLn "Wrong data"
				WS.sendClose conn ("" :: Text)							
		where disconnect c = do
					putStrLn $ "Disc user " ++ (show $ fst c )		
					liftIO $ modifyMVar_ state $ \s -> do
						let newState = removeUser c s
						pushUserList newState
						return newState

handleUser :: WS.Connection -> MVar ServerState -> Text -> IO ()
handleUser conn state nick = do
				users <- liftIO $ readMVar state
				pushUserList users
				forever $ do
					msg <- WS.receiveData conn
					let json = decode msg :: Maybe SDP
					case json of
						Just s -> do
							let who = target s
							users <- liftIO $ readMVar state
							let c = getConnection who users
							case c of
								Just co -> WS.sendTextData co $ sdp s
								Nothing -> WS.sendClose conn ("" :: Text)
						Nothing -> do
							putStrLn "Did not get SDP"

pushUserList :: ServerState -> IO ()
pushUserList users = do
    let users' = encode $ UserList $ map fst users
    forM_ users $ \u -> WS.sendTextData (snd u) users'																

--API for videochating [] path Nothing status100
openVideo :: String -> Int -> IO ()
openVideo path prt = runUrlPort prt path  $  apply 
	where
		apply :: Application
		apply _ respond = withFile "WebClient.html" ReadMode $ \handle ->
					respond $ responseStream
					status200 ([("Content-Type","text/html")]) $ \send _flush -> do
												let loop = do 
														bs <- B8.hGet handle 4096
														if B8.null bs
															then return ()
															else send (fromByteString bs) >> loop
												   in loop			
		--apply :: Application
		--apply _ respond = respond $
		--		responseFile status200 ([("Content-Type","text/html")]) "1.html"
		--		Nothing
  --				responseStream status200 ([("Content-Type","text/html")]) $ \send flush -> do
  --																				send $ "Started///\n"
  --																				flush
  --																				threadDelay 100000
  --																				send $ "all donne"
  --				--responseLBS status200 ([("Content-Type","text/html")]) "<video id=vid></video><script type=text/javascript>var video=document.getElementById('vid'); navigator.getUserMedia=navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.msGetUserMedia || window.URL.mozCreateObjectURL || window.URL.msCreateObjectURL; navigator.getUserMedia({video:true,audio:true},function (stream){videoStreamUrl = window.URL.createObjectURL(stream);video.src=videoStreamUrl;},function(){});</script>"

------------------------------------------------------------------------------------------------------new	
data TypeCon = Translation | Data | Chating
--   member of network = name adress type of con datqbaseNumber
type Member = Member { name :: string,
					   handbook :: WorkTracking a,	
					   typeCon :: TypeCon,
					   sqlNum :: Int }		
data WorkTransactions a b = WorkTransactions a b | Nothing	
data Videotical = Member (Chan a) 

serv :: Int -> Int -> IO ()
serv countMembers port = withSocketsDo $
			adrese <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing $ Just 4500
			let neededAddr = head adrese
			bindedSocket <- socket $ addrFamily neededAddr $ Stream defaultProtocol
			bind bindedSocket (addrAddress neededAddr)
			listen countMembers
			groupTrackFunc [] 

		where	
			groupTrackFunc membrLibrary  = do
						forkIO $ interaction membrLibrary
						(membrSocket, membrSocketAddr) <- accept bindedSocket
						details <- recv sock 150	
						[name,hb,typec,num] = split details  
						groupTrackFunc $ membrLibrary : Member { name, hb, typec, num }  

interaction :: [Member] -> 	