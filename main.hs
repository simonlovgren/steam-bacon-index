{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson
-}

import BreadthFirstSearch
import FifoQueue
import IDSearchTree
import Test.HUnit
{-
main SteamID1 SteamID2
PURPOSE		: Request inputs and check validity before sending to breadthFirstSearch.
PRE 		: True
POST 		: Returns the 'Kevin Bacon Index' between the two supplied SteamIDs
EXAMPLES 	: 76561197999847293 76561197988295792 = 
			  76561197999847293 has 13 friends, current depth 1 I found it!! 0 people between the ids
			  76561197999847293 1123 = Faulty input
-}
main :: IO ()
main = do  
		putStrLn "Please enter first Steam ID"
		rawID1 <- getLine
		putStrLn "Please enter second Steam ID"
		rawID2 <- getLine
		putStrLn ("IDs are " ++ rawID1 ++ ", " ++ rawID2 ++ ".") 
		if (isSteamID rawID1) && (isSteamID rawID2) && (rawID1 /= rawID2)
			then do
				let id1 = (read rawID1::Integer) 
				let	id2 = (read rawID2::Integer)
				putStrLn ("IDs are:\n" ++ rawID1 ++" and "++rawID2)
				breadthFirstSearch (queue (id1,[]) EmptyQ) id2 Empty 
	 	else do 
			putStrLn "Expected input; Two non identical 17 Char long numerical strings"
	 		
	 	return ()

{-
isReadableString someString
PURPOSE		: To determine if all elements in supplied String are numerals.
PRE 		: A String
POST 		: True if all elements are numerals and no element is zero, else False.
EXAMPLES 	: isReadableString "123" = True
			  isReadableString "abc" = False
			  isReadableString "100" = False
-}
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
			isReadableString' ('0':xs) dotFound = True : (isReadableString' xs dotFound)
			isReadableString' ('.':xs) True		= False: (isReadableString' xs True)
			isReadableString' ('.':xs) _	    = True : (isReadableString' xs True)
			isReadableString' ( _ :xs) _ 		= False: []
		in 
			all (==True)(isReadableString' s False)

{-
isSteamID someString
PURPOSE		: To determine if a supplied string is of correct length and returns a True from isReadableString.
PRE 		: A String
POST 		: A True if string is of length 17 and contains only numerals. Else False.
EXAMPLES 	: isSteamID "12345678910111213" = True
			  isSteamID "123456" = False
			  isSteamID "abcdef" = False
-}
isSteamID :: String -> Bool 
isSteamID id = (length id) >= 17 && (isReadableString id) 



--Test 1; isReadableString with valid input.
mainTest1 = TestCase (assertEqual "for (isRedableString 123)," True (isReadableString "123")) 

--Test 2; isReadableString with NON-valid input.
mainTest2 = TestCase (assertEqual "for (isRedableString abc)," False (isReadableString "abc")) 

--Test 3; isSteamID with valid input.
mainTest3 = TestCase (assertEqual "for (isSteamID 12345678911234567)," True (isSteamID "12345678911234567"))

--Test 4; isSteamID with NON-valid input, one or more element = zero.
mainTest4 = TestCase (assertEqual "for (isSteamID 10340678011230560)," False (isSteamID "10340678011230560"))

--Test 5; isSteamID with NON-valid input, string is not of length 17.
mainTest5 = TestCase (assertEqual "for (isSteamID 123)," False (isSteamID "123"))



mainTests = TestList [mainTest1, mainTest2, mainTest3, mainTest4, mainTest5]




