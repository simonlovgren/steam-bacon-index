module BreadthFirstSearch where 

import SteamAPI
import FifoQueue
import IDSearchTree
import SteamFriends


{-
breadthFirstSearch q g t
PURPOSE		: Loops through the queue and terminates if queue is empty or looked for id is found.
PRE 		: 
POST 		:
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
      aList <- getIDs steamid
      --debugg output
      putStrLn (show steamid ++ " has " ++ show(length aList) ++ " friends, current depth " ++ show(length route + 1))
      --check if one of the ids connected to the id in the queue is the one searched for.
      if not(goalIdReached aList goal) then do
        -- Add ids not yet visited to the queue and start over.
        let (newQueue, visitedUpdated) = checkAndAdd queue (steamid:route) visited aList
        breadthFirstSearch newQueue goal visitedUpdated
        else do
        putStrLn ("I found it!! " ++ show (length route) ++ " people between the ids")

{-

PURPOSE		: Checks if id has already been visited or adds it to the queue unless the termination depth has been reached.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
checkAndAdd :: SimpleQueue ((Integer, [Integer])) -> [Integer] -> Tree Integer  -> [Integer] -> (SimpleQueue ((Integer, [Integer])), Tree Integer)
checkAndAdd q route prevVisited [] = (q,prevVisited)
checkAndAdd q route  prevVisited (id:remainingId)
  | not isVisited && not(depthReached route) = checkAndAdd (queue (id,route) q) route visitedUpdated remainingId
  | otherwise = checkAndAdd q route visitedUpdated remainingId
    where
      (visitedUpdated, isVisited) = searchTuple prevVisited id

{-

PURPOSE		: Check if the looked for id has been found.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
goalIdReached [] _ = False
goalIdReached (x:xs) goal
  | x == goal = True
  | otherwise = goalIdReached xs goal

{-

PURPOSE		: Checks if certain depth has been reached by the graph algorithm.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
depthReached :: [Integer] -> Bool
depthReached list = if (length list + 1) < 5 then False else True


--halvfärdiga testcase
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
  breadthFirstSearch q 76561197999847293 Empty

--testing checkAndAdd
test4 = checkAndAdd EmptyQ [] Empty [76561197989194839,76561198000124224,76561198043343260]

test5 = do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 1 Empty

test6 = do
  testid <- test
  let q = queue (testid,[]) EmptyQ
  breadthFirstSearch q 76561198015054781 Empty
