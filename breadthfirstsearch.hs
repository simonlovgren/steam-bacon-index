module BreadthFirstSearch where 

import Steam
import FifoQueue
import IDSearchTree
import Test.HUnit
import Control.Monad
import Control.Exception


{-
breadthFirstSearch q g t
PURPOSE		: Loops through the queue and terminates if queue is empty or looked for id is found.
PRE 		: True
POST 		: Returns a list with blablabla
EXAMPLES 	:
SIDEEFFECTS     : Plenty
-}
breadthFirstSearch :: SimpleQueue ((Integer,[Integer])) -> Integer -> Tree Integer -> IO ()
breadthFirstSearch q goal visited = do
  -- check if the queue is empty, which means the id was not found in the network.
  if (isQueueEmpty q) then do
    putStrLn "Didnt find any match before queue empty"
    else do
      --get the next id in the queue and ids on steam connected to it.
      let (entry, queue) = deQueue q
          (steamid, route) = (fst entry, snd entry)
      aList <- friendsIDs steamid
      --debugg output
      putStrLn (show steamid ++ " has depth " ++ show(length route + 1))
      -- Add ids not yet visited to the queue and start over.
      let (newQueue, visitedUpdated, goalReached) = checkAndAdd queue (steamid:route) visited aList goal
      --check if one of the ids connected to the id in the queue is the one searched for.
      if not goalReached then do
        breadthFirstSearch newQueue goal visitedUpdated
        else do
        putStrLn ("I found it!! " ++ show (length route) ++ " people between the ids")

{-
checkAndAdd q r v i g
PURPOSE		: Checks if id is the one searched for and adds ids not yet visited to queue.
PRE 		: True
POST 		: Adds not visited ids in i to queue q and tree of visited v if depth of search not yet reached, or returns True if g is found in list i.
EXAMPLES 	:		
-}
checkAndAdd :: SimpleQueue ((Integer, [Integer])) -> [Integer] -> Tree Integer  -> [Integer] -> Integer -> (SimpleQueue ((Integer, [Integer])), Tree Integer, Bool)
checkAndAdd q route prevVisited idList goal
  | not (length route + 1 < 5) = (q, prevVisited, goalIdReached idList goal)
  | otherwise = checkAndAdd_aux q route prevVisited idList goal
    where
      checkAndAdd_aux q route prevVisited [] goal = (q,prevVisited, False)
      checkAndAdd_aux q route prevVisited (id:remainingId) goal
        | id == goal = (q, prevVisited, True)
        | (not isVisited) = checkAndAdd_aux (queue (id,route) q) route visitedUpdated remainingId goal
        | otherwise = checkAndAdd_aux q route visitedUpdated remainingId goal
          where
            (visitedUpdated, isVisited) = searchTuple prevVisited id


{-
goalIdReached i g
PURPOSE		: Check if the looked for id has been found.
PRE 		: True
POST 		: Returns True if g is in list i.
EXAMPLES 	: 
-}
goalIdReached [] _ = False
goalIdReached (x:xs) goal
  | x == goal = True
  | otherwise = goalIdReached xs goal

--testcases
testValue :: IO Integer
testValue = return(76561198028357851)
{-
--Check if goal reached
test1 = do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 76561198000124224 Empty

--Check if queue empty
test2 = do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch EmptyQ 1 Empty
-}
--Check other...
test3 =  do
  testid <- testValue
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 76561197962270956 Empty
{-
--testing checkAndAdd
test4 = checkAndAdd EmptyQ [] Empty [76561197989194839,76561198000124224,76561198043343260]
-}
--test if goal id is not found
test5 = do
  testid <- testValue
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 1 Empty
{-
--test a bit down
test6 = do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 76561198015054781 Empty
-}



--testBfs1 = TestCase $ assertBool "Test 

runBfsTests = runTestTT $ TestList []
