{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson
-}

module BreadthFirstSearch where 

--standard modules
import Test.HUnit

--"homemade" modules
import Steam
import FifoQueue
import IDSearchTree
import KeyVal



{-
breadthFirstSearch q g t
PURPOSE		: Gathers friend lists from steam, starting with the first value in the queue, and searches them for the goal id. Terminates with a success message if id is found or
                  a failure message if id not found withina depth of five in the friend network or if all friends in the network has been searched.
PRE 		: True
POST 		: True
EXAMPLES 	: N/A
SIDEEFFECTS     : Displays current ID being investigated and the final result of the breadth first search function.
-}
breadthFirstSearch :: SimpleQueue ((Integer,[Integer])) -> Integer -> Tree Integer -> IO ()
breadthFirstSearch q goal visited = do
  -- check if the queue is empty, which means the id was not found in the network.
  if (isQueueEmpty q) then do
    putStrLn "Didnt find any match before reaching the end of the network or too many degrees of separation"
    else do
      --get the next id in the queue and ids on steam connected to it.
      let (entry, queue) = deQueue q
          (steamid, route) = (fst entry, snd entry)
      aList <- friendsIDs steamid
      --show what the program is working on.
      putStrLn (show steamid ++ " has depth " ++ show(length route + 1))
      -- Check if any of the ids is the one we are looking for and add not yet visited to the queue unless termination depth has been reached.
      let (newQueue, visitedUpdated, goalReached) = checkAndAdd queue (steamid:route) visited aList goal
      --check if one of the ids connected to the id in the queue is the one searched for.
      if not goalReached then do
        breadthFirstSearch newQueue goal visitedUpdated
        else do
        putStrLn ("Id found, " ++ show (length (route)) ++ " id between them.")
        successRoute (goal:steamid:route)

{-
checkAndAdd q r v i g
PURPOSE		: Checks if id is the one searched for and adds ids not yet visited to queue.
PRE 		: True
POST 		: Adds not visited ids in i to queue q and tree of visited v if depth of search not yet reached, or returns True if g is found in list i.
EXAMPLES 	: checkAndAdd EmptyQ [] Empty [1,2,3] 0        = (SQ [(1,[])] [(3,[]),(2,[])],Branch 1 Empty (Branch 2 Empty (Branch 3 Empty Empty)),False)
                  checkAndAdd EmptyQ [] Empty [1,2,3] 1        = (EmptyQ,Empty,True)
                  checkAndAdd EmptyQ [1,2,3,4] Empty [1,2,3] 0 = (EmptyQ,Empty,False)
-}
checkAndAdd :: SimpleQueue ((Integer, [Integer])) -> [Integer] -> Tree Integer  -> [Integer] -> Integer -> (SimpleQueue ((Integer, [Integer])), Tree Integer, Bool)
checkAndAdd q route prevVisited idList goal
  -- check if termination depth has been reached and if so check if goal has been reached
  | not (length route + 1 < 5) = (q, prevVisited, goalIdReached idList goal)
  | otherwise = checkAndAdd_aux q route prevVisited idList goal
    where
      {-
      checkAndAdd q r v i g
      PURPOSE		: Checks if id is the one searched for and adds ids not yet visited to queue.
      PRE 		: True
      POST 		: Adds not visited ids in i to queue q and tree of visited v if depth of search not yet reached, or returns True if g is found in list i.
      EXAMPLES 	        : checkAndAdd EmptyQ [] Empty [1,2,3] 0 = (SQ [(1,[])] [(3,[]),(2,[])],Branch 1 Empty (Branch 2 Empty (Branch 3 Empty Empty)),False)
                          checkAndAdd EmptyQ [] Empty [1,2,3] 1 = (EmptyQ,Empty,True)
      -}
      checkAndAdd_aux q route prevVisited [] goal = (q,prevVisited, False)
      checkAndAdd_aux q route prevVisited (id:remainingId) goal
        -- check if id is the id we are looking for, if so return True
        | id == goal = (q, prevVisited, True)
        -- Otherwise if id has not been visited, add it to the tree with visited ids and the queue
        | (not isVisited) = checkAndAdd_aux (queue (id,route) q) route visitedUpdated remainingId goal
        -- if id has been visited move on to next id in the list
        | otherwise = checkAndAdd_aux q route visitedUpdated remainingId goal
          where
            (visitedUpdated, isVisited) = searchTuple prevVisited id

{-
successRoute xs
PURPOSE		: Gets player nicknames from Steam and prints them in order.
PRE 		: True
POST 		: True
SIDE EFFECTS    : Prints the route between the two investigated ids to the console.
EXAMPLES 	: N/A		
-}
successRoute :: [Integer] -> IO ()
successRoute idList = do
  names <- Steam.playerNamesOrdered idList
  print names
  putStrLn $ makeStringOfRoute names
  
{-
makeStringOfRoute xs
PURPOSE		: Gets player nicknames from Steam and prints them in order.
PRE 		: True
POST 		: Returns a string with the elements in xs concatenated.
EXAMPLES 	: makeStringOfRoute ["one","two","three"] = "From three to two to one"
                  makeStringOfRoute [Nothing,"two","three"] = "From three to two to unknown"	
-}
makeStringOfRoute :: [Maybe String] -> String
makeStringOfRoute [] = []
makeStringOfRoute ((Just name):[]) = "From " ++  name
makeStringOfRoute ((Nothing):[]) = "From unknown"
makeStringOfRoute ((Nothing):names) = makeStringOfRoute names ++ " to unknown"
makeStringOfRoute ((Just name):names) = makeStringOfRoute names ++ " to " ++ name
  

{-
goalIdReached i g
PURPOSE		: Check if the looked for id has been found.
PRE 		: True
POST 		: Returns True if g is in list i.
EXAMPLES 	: N/A
-}
goalIdReached :: Eq a => [a] -> a -> Bool
goalIdReached [] _ = False
goalIdReached (x:xs) goal
  | x == goal = True
  | otherwise = goalIdReached xs goal


--Testcases

testBfs1 = TestCase $ assertBool "checkAndAdd: Test if initial insertion to tree and queue works when goal is not found" 
           (let
              (q,tree,goal) = checkAndAdd EmptyQ [] Empty [1,2,3] 0
            in
              q == (SQ [(1,[])] [(3,[]),(2,[])]) && tree == (Branch 1 Empty (Branch 2 Empty (Branch 3 Empty Empty))) && goal == False)

testBfs2 = TestCase $ assertBool "checkAndAdd: Test if initial insertion to tree and queue works when goal is found" 
           (let
              (q,tree,goal) = checkAndAdd EmptyQ [] Empty [1,2,3] 1
            in
             goal == True)

testBfs3 = TestCase $ assertBool "checkAndAdd: Test if initial insertion to tree and queue works when goal is found" 
           (let
              (q,tree,goal) = checkAndAdd EmptyQ [] (Branch 1 Empty (Branch 2 Empty (Branch 3 Empty Empty))) [1] 0
            in
               q == EmptyQ && tree == (Branch 1 Empty (Branch 2 Empty (Branch 3 Empty Empty))) && goal == False)

testBfs4 = TestCase $ assertBool "checkAndAdd: Test if length termination reached, but goal not found" 
           (let
              (q,tree,goal) = checkAndAdd EmptyQ [1,2,3,4] Empty [1,2,3] 0
            in
              q == EmptyQ && tree == Empty && goal == False)

testBfs5 = TestCase $ assertBool "checkAndAdd: Test if length termination reached, and goal found" 
           (let
              (q,tree,goal) = checkAndAdd EmptyQ [1,2,3,4] Empty [1,2,3] 1
            in
              q == EmptyQ && tree == Empty && goal == True)

testBfs6 = TestCase $ assertBool "makeStringOfRoute: Test if string is correctly created"
           (let
              result = makeStringOfRoute [Just "one",Just "two",Nothing,Just "three"]
            in
             result == "From three to unknown to two to one")

testBfs7 = TestCase $ assertBool "makeStringOfRoute: Test if string is correctly created if first element is Nothing"
           (let
              result = makeStringOfRoute [Just "one",Just "two",Just "three", Nothing]
            in
             result == "From unknown to three to two to one")

runBfsTests = runTestTT $ TestList [testBfs1,testBfs2,testBfs3,testBfs4, testBfs5, testBfs6, testBfs7]

