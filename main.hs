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
import Steam
import KeyVal
{-
main SteamID1 SteamID2
PURPOSE		: Request inputs and check validity before sending to breadthFirstSearch.
PRE 		: True
POST 		: True
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
isReadableString s
PURPOSE		: Check if all elements in s are numerals.
PRE 		: True
POST 		: Returns True if all elements in s are numerals, else False.
EXAMPLES 	: isReadableString "123" = True
			  isReadableString "abc" = False
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
		isReadableString' ( _ :xs) _ 		= False: []
	in 
		all (==True)(isReadableString' s False)

{-
isSteamID s
PURPOSE		: Check if a s is of length 17 and convertable to numeral.
PRE 		: True
POST 		: Returns True if s is of length 17 and s is convertable to numerals. Else False.
EXAMPLES 	: isSteamID "12345678910111213" = True
			  isSteamID "123456" = False
			  isSteamID "abcdef" = False
-}
isSteamID :: String -> Bool 
isSteamID id = (length id) >= 17 && (isReadableString id) 








runAllTestsProject = do
    putStrLn ("Running all tests that do not require access to remote Steam data")
    putStrLn ("Running Queue tests")
    runQueueTests
    putStrLn ("Running IDSearchtree tests")
    runTestTT iDSTtests
    putStrLn ("Running Breadth Width First tests")
    runBfsTests
    putStrLn ("Running Main tests")
    runTestTT mainTests
    putStrLn ("Running KeyVal tests")
    runTestTT tests
    putStrLn ("Running Steam API tests")
    putStrLn ("Running Request tests")
    return()   


--Test 1; isReadableString with valid input.
mainTest1 = TestCase (assertEqual "for (isRedableString 123)," True (isReadableString "123")) 

--Test 2; isReadableString with NON-valid input.
mainTest2 = TestCase (assertEqual "for (isRedableString abc)," False (isReadableString "abc")) 

--Test 3; isSteamID with valid input.
mainTest3 = TestCase (assertEqual "for (isSteamID 12345678911234567)," True (isSteamID "12345678911234567"))

--Test 4; isSteamID with NON-valid input, one or more element non numeral.
mainTest4 = TestCase (assertEqual "for (isSteamID abc40678011230560)," False (isSteamID "abc40678011230560"))

--Test 5; isSteamID with NON-valid input, string is not of length 17.
mainTest5 = TestCase (assertEqual "for (isSteamID 123)," False (isSteamID "123"))



mainTests = TestList [mainTest1, mainTest2, mainTest3, mainTest4, mainTest5]




