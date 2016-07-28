module CommandsControls
where
import System.Environment
import Control.Concurrent
import Data.Text (split,unpack,pack, toLower,words,Text)

main :: IO ()
main = do 
	putStrLn "Enter your Name"
	login <- getLine 
	putStrLn $ "Welcome " ++ login ++ "\nEnter some command! List of commands - !List"
	commandController

 
	
	
commandController :: IO ()
commandController = do 
				commands <- getLine 
				putStrLn $ sequre commands
				return ()
	 
	 where sequre word 
	 			| firsArg == "" = "Empty List!Try again!"
	  			| firsArg == "connect" = "Connecting..."
				| firsArg == "host" = "Channel created!"
			 	| firsArg == "q" = "Exiting..."
				| firsArg == "!list" = "Connect hostname port - connect to channel \nHost port - start your channel \nq - exiting"
				| otherwise = "Wrong command.Try again!"  	
				where firsArg = unpack $ toLower $ head $ Data.Text.words $ pack word
        



