import BreadthFirstSearch
import FifoQueue
import IDSearchTree

main :: IO ()
main = do  
		putStrLn "Please enter first Steam ID"
		rawID1 <- getLine
		putStrLn "Please enter second Steam ID"
		rawID2 <- getLine
		putStrLn ("IDs are " ++ rawID1 ++ ", " ++ rawID2 ++ ".")
		
		if (isSteamID rawID1) && (isSteamID rawID2)
			then do
				let id1 = (read rawID1::Integer) 
				let	id2 = (read rawID2::Integer)
				putStrLn ("IDs are:\n" ++ rawID1 ++"\n"++rawID2)
				breadthFirstSearch (queue (id1,[]) EmptyQ) id2 Empty 
	 	else do 
	 		putStrLn "Faulty input"
	 		
	 	return ()

isReadableString :: String -> Bool
isReadableString s =
		let 
			isReadableString' :: String -> Bool -> [Bool]
			isReadableString' [] _ = []
			isReadableString' ('1':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('2':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('3':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('4':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('5':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('6':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('7':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('8':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('9':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('.':xs) True		= False: (isReadableString' xs True)
			isReadableString' ('.':xs) _	    = True : (isReadableString' xs True)
			isReadableString' ( _ :xs) _ 		= False: []
		in 
			all (==True)(isReadableString' s False)

isSteamID :: String -> Bool 
isSteamID id = (length id) >= 17 && (isReadableString id) 


