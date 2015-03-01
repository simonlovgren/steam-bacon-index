module FifoQueue where

{-Data representation
Representation convention: A "first in, first out" queue.
Representation invariant: The order which elements in the queue are removed shall be in the order they were added.
Comment:
Two list fifo queue concept found at http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf
-}            
data SimpleQueue a = EmptyQ | SQ [a] [a] deriving Show

{- createQueue
Purpose : Creates a new queue.
Pre     : True
Post    : Returns an empty  queue.
Example : N/A
-}
createQueue :: SimpleQueue a
createQueue = EmptyQ

{- deQueue q
Purpose : Returns the item in the front of the queue and removes it from the queue.
Pre     : q may not be empty.
Post    : Returns the item in the front of q and the rest of the queue.
Examples: deQueue (SQ [1,2,3] [] = (1,(SQ [2,3] []))
-}
deQueue :: SimpleQueue a -> (a, SimpleQueue a)
deQueue (SQ (x:[]) []) = (x, EmptyQ)
deQueue q@(SQ [] rearList) = deQueue (shiftQueue q)
deQueue (SQ (x:frontList) rearList) = (x, (SQ frontList rearList))

{- queue x q
Purpose : Adds an item to a queue.
Pre     : True
Post    : Returns q with x inserted.
Examples: queue x (EmptyQ) = (SQ [x] [])
          queue x (SQ [1,2,3] [6,5,4] = (SQ [1,2,3] [x,6,5,4]
-}
queue :: a -> SimpleQueue a -> SimpleQueue a
queue x EmptyQ = (SQ [x] [])
queue x (SQ frontList rearList) = (SQ frontList (x:rearList))

{- shiftQueue q@(SQ fL rL)
Purpose : Rearranges items in the queue to facilitate easy dequeue.
Pre     : True
Post    : Returns q with all values in in the queue placed in fL if fL was empty.
Examples: shiftQueue q@(SQ [] [3,2,1] = (SQ [1,2,3] [])
-}
shiftQueue :: SimpleQueue a -> SimpleQueue a
shiftQueue EmptyQ = EmptyQ
shiftQueue (SQ [] rearlist) = (SQ (reverse rearlist) [])
shiftQueue q = q

{- isQueueEmpty q
Purpose : Check if the queue is empty.
Pre     : True
Post    : Returns true or false depending on if q is empty.
Examples: N/A
-}
isQueueEmpty :: SimpleQueue a -> Bool
isQueueEmpty EmptyQ = True
isQueueEmpty _ = False

