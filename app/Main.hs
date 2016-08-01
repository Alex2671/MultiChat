module Main where

import Lib
import System.IO

main :: IO ()
main = do 
	setConsole 
	--openVideo "C:/Applications/stack/Multichat/.stack-work/dist/x86_64-windows/Cabal-1.22.4.0/build/Multichat-exe/1.html"
	putStrLn "Enter your Name"
	nick <- getLine 
	putStrLn $ "Welcome " ++ nick ++ "\nEnter some command! List of commands - !List"
	nextStep <- commandController nick
	return ()




 
	
	

        

