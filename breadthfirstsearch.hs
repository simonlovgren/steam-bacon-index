import SteamAPI
import FifoQueue
import IDSearchTree
import SteamFriends
import Debug.Trace

test :: IO Integer
test = return(76561198028357851)

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

--Check other...
test3 =  do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 76561197966095937 Empty

--testing checkAndAdd
test4 = checkAndAdd EmptyQ [] Empty [76561197989194839,76561198000124224,76561198043343260]

{-

PURPOSE		: Loops through the queue and terminates if queue is empty or looked for id is found.
PRE 		: 
POST 		:
EXAMPLES 	:
SIDEEFFECTS     : Plenty
-}
breadthFirstSearch :: SimpleQueue ((Integer,[Integer])) -> Integer -> Tree Integer -> IO ()
breadthFirstSearch q goal visited = do
  if (isQueueEmpty q) then do
    putStrLn "Didnt find any match before queue empty"
    else do
      let (entry, queue) = dequeue q
          (steamid, route) = (fst entry, snd entry)
      aList <- getIDs steamid
      print steamid
      if not(checkGoalReached aList goal) then do
        let (newQueue, visitedUpdated) = checkAndAdd queue (steamid:route) visited aList
        breadthFirstSearch newQueue goal visitedUpdated
        else do
        putStrLn "I found it!!"

{-

PURPOSE		: Checks if id has already been visited or adds it to the queue unless the termination depth has been reached.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
checkAndAdd :: SimpleQueue ((Integer, [Integer])) -> [Integer] -> Tree Integer  -> [Integer] -> (SimpleQueue ((Integer, [Integer])), Tree Integer)
checkAndAdd q route prevVisited [] = (q,prevVisited)
checkAndAdd q route  prevVisited (id:remainingId)
  | not isVisited && not(checkDepthReached route) = checkAndAdd (queue (id,route) q) route visitedUpdated remainingId
  | otherwise = checkAndAdd q route visitedUpdated remainingId
    where
      (visitedUpdated, isVisited) = searchTuple prevVisited id
{-

PURPOSE		: Placeholder function to fake JSON parser output.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
parseryparsery :: IO String -> IO [Integer]
parseryparsery x = return ([76561197989194839,76561198000124224,76561198043343260])

{-

PURPOSE		: Check if the looked for id has been found.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
checkGoalReached [] _ = False
checkGoalReached (x:xs) goal
  | x == goal = True
  | otherwise = checkGoalReached xs goal

{-

PURPOSE		: Checks if certain depth has been reached by the graph algorithm.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
checkDepthReached :: [Integer] -> Bool
checkDepthReached list = if (length list + 1) < 5 then False else True

