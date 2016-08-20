 {-# LANGUAGE OverloadedStrings #-}   	
 {-# LANGUAGE TamplateHaskell #-} 
 {-# LANGUAGE QuasiQuotes #-} 
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
import Database.PostegreSQL.Simple

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


type NickName = B8.ByteString
data TypeOfData = Doc | Txt | IMG | Other deriving (Show)
instance Show TypeOfData

data TypeCon = Translation | Data | Chating
--   member of network = name adress type of con datqbaseNumber
data Member = Member { name :: String,
					   takeSocket :: SockAddr,	
					   handbook :: WorkTransactions a,	
					   typeCon :: TypeCon,
					   sqlNum :: Int }		

data WorkTransactions a b = a b WorkTransactions a b | End   	
data Videotical = Member (Chan a) 
=======


getConnectString

serv :: Int -> Int -> IO ()
serv countMembers port = withSocketsDo $
			adrese <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing $ Just 4500
			let neededAddr = head adrese
			bindedSocket <- socket $ addrFamily neededAddr $ Stream defaultProtocol
			bind bindedSocket (addrAddress neededAddr)
			conn <- connect {"localhost", 5432, "postgres", , "haskellDatas"}
			listen countMembers
			datalist <- query conn [sql| select ID_St from ID_Student, select Name_Student form ID_Student;|] :: IO [Maybe (Int,String)] 
			fullLibrary <- getMemberLibrary conn
			guiDRAWLibrary fullLibrary
			handleMessages bindedSocket fullLibrary
		where
			getMemberLibrary :: [Maybe (Int,String)] -> Connection -> [Member]	
			getMemberLibrary [Nothing] conn =  sequence $ foldl \list -> forkIO . getMembersAttributes <*> list $ groupTrackFunc bindedSocket []										
			getMemberLibrary [(Just (id,name)):xs] conn =  id $ name $ \(idd,namee) ->[]:Member { name,(getBook idd conn),Data,idd }:getMemberLibrary xs conn 
			handleMassages :: Socket -> [Member] -> IO () 
			handleMassages sock library = recvFrom sock 150 >>= addRemoveDataFunc library >> handleMassages 
				where
					getBook id con = creating WorkTransactions $ execute con [sql|select * from Work join Datee on Work.IdWork = Datee.IdWork where ID_Student = id|] :: IO [Maybe (Int,Int,String,String,ByteString)]
				   	groupTrackFunc socket xs = xs : accept socket
					addRemoveDataFunc :: (String, Int, SockAddr) -> [Member] -> [Member]
					addRemoveDataFunc _ [] = return $ printf "No one member has connect"
					addRemoveDataFunc (dat,nbytes,addres) a = case first . split dat of  
														"Remove" -> removeLib 
														"Add" -> addLib 							
				  	
getMembersAttributes :: (Socket,SockAddr) -> IO Member
getMembersAttributes (s,sa) = do
								name <- recv s 500 
								return $ Member {name, sa, Nothing, Data, 1}


addLib :: (String, Int, SockAddr) -> [Member] -> [Member]
addLib mes@(dataa,nbytes,addrss) [(Member {name, sockAddress, notebook, con, sql}):xs] = if addrss == sockAddress then addToHandBook $ notebook $ tail . split " " dataa
																						     else addLib mes xs 								
				where 
				   addToHandBook :: WorkTransactions TypeOfData String -> [String] -> [Member]
				   addToHandBook book [typ:date] = case typ of
												   "Doc" -> inTheEnd book date Doc  
												   "Txt" -> inTheEnd book date Txt
												   "IMG" -> inTheEnd book date IMG
												   "Other" ->	inTheEnd book date Other
		           inTheEnd :: WorkTransactions TypeOfData String -> String -> TypeOfData -> WorkTransactions TypeOfData String
		           inTheEnd (tpe x (End)) date typee = tpe x date () 
		           inTheEnd (_ x) date typee = inTheEnd x date typee     										   

removeLib :: (String, Int, SockAddr) -> [Member] -> [Member] 
removeLib mes@(dataa,nbytes,addrss) list@[(Member {name, sockAddress, notebook, con, sql}):xs] = if addrss == sockAddress then removeFromHandBook notebook (tail . split " " dataa) 
																						    else removeLib mes xs				
				where 
				   removeFromHandBook :: WorkTransactions TypeOfData String -> [String] -> [Member]	
				   removeFromHandBook _ _ () _ = []																	    	
				   removeFromHandBook typ dat sa [typ:name] = if name == dat then sa else removeFromHandBook sa

addAllData :: [Member] -> Connection -> IO ()
addAllData asa@[(Member {name, sockAddress, notebook, con, sql}):xs] con = do $
															idStudentsinDB <- recurseAdd con asa  
												where
												   recurseAdd con [(Member {name, sockAddress, notebook, con, sql}):xs] = 

checkLessonWork :: 

sendMessageWork ::												   	

getDataStream :: 

guiDRAWLibrary ::








































































