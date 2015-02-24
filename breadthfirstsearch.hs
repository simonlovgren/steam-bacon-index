import SteamAPI
import FifoQueue

test :: IO Integer
test = return(76561198028357851)
  
main = do
  let q = queue (test,[]) Empty
  breadthFirstSearch q test []


breadthFirstSearch :: SimpleQueue ((IO Integer,[Integer])) -> IO Integer -> [Integer] -> IO ()
breadthFirstSearch q goal visited = do
  if (isQueueEmpty q) then do
    return ()
    else do
      let (entry, queue) = dequeue q
          (steamid, route) = (fst entry, snd entry)
      lookupId <- steamid
      (current,aList) <- parseryparsery $ getFriendList lookupId
      if not(checkGoalReached aList goal) then do
        breadthFirstSearch_aux q goal visited aList
        else do
        return()


parseryparsery :: IO String -> IO (IO Integer, [IO Integer])
parseryparsery x = return (test, [test,test,test])


checkGoalReached [] _ = False
checkGoalReached (x:xs) goal
  | x == goal = True
  | otherwise = checkGoalReached xs goal


checkDepthReached :: [Integer] -> Bool
checkDepthReached list = if (length list + 1) < 5 then False else True

breadthFirstSearch_aux :: SimpleQueue ((IO Integer, [Integer])) -> IO Integer -> [Integer] ->[IO Integer] -> IO ()
breadthFirstSearch_aux q goal visited toVisit = undefined
  
