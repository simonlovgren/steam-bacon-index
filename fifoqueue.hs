module FifoQueue where

{-Data representation
Two list fifo queue concept found at http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf
-}            

data SimpleQueue a = Empty | SQ [a] [a] deriving Show

dequeue :: SimpleQueue a -> (a, SimpleQueue a)
dequeue (SQ (x:[]) []) = (x, Empty)
dequeue q@(SQ [] rearList) = dequeue (shiftQueue q)
dequeue (SQ (x:frontList) rearList) = (x, (SQ frontList rearList))

queue :: a -> SimpleQueue a -> SimpleQueue a
queue x Empty = (SQ [x] [])
queue x (SQ frontList rearList) = (SQ frontList (x:rearList))

shiftQueue :: SimpleQueue a -> SimpleQueue a
shiftQueue (SQ [] rearlist) = (SQ (reverse rearlist) [])
shiftQueue q = q

isQueueEmpty :: SimpleQueue a -> Bool
isQueueEmpty Empty = True
isQueueEmpty _ = False

