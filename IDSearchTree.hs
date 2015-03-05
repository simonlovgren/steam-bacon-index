{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson
-}

module IDSearchTree where 
import Test.HUnit


{-
REPRESENTATION CONVENTION: A binary tree with key-value Integer and a left and a right sub-tree
REPRESENTATION INVARIANT: Every element in the left sub-tree is smaller than the key-value of its parent tree.
						  Every element in the right sub-tree is larger than the key-value of its parent tree.
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show, Eq)
{-
searchTuple t b 
PURPOSE		: Check if b is already in tree, or if not insert it.
PRE 		: True
POST 		: Returns tuple with True and t if b found in t, or False and t with b inserted if not found.
EXAMPLES 	: searchTuple (Branch 10 Empty Empty) 9 = (Branch 10 (Branch 9 Empty Empty) Empty,False)

-}
searchTuple :: Tree Integer -> Integer -> (Tree Integer, Bool)
searchTuple Empty b = ((isInIDSearchTree Empty b),(searchIDTree Empty b))
searchTuple (Branch a left right) b = ((isInIDSearchTree (Branch a left right) b ),(searchIDTree (Branch a left right) b))

{-
searchIDTree t b
PURPOSE		: Check if b in t.
PRE 		: True
POST 		: Return a True is b is found in t and a False if not.
EXAMPLES 	: searchIDTree (Branch 10 (Branch 9 Empty Empty) (Branch 12 Empty Empty)) 12 = True	
			  searchIDTree (Branch 10 (Branch 9 Empty Empty) (Branch 12 Empty Empty)) 11 = False		
-}
searchIDTree :: Tree Integer -> Integer -> Bool
searchIDTree Empty b = False 
searchIDTree (Branch a left right) b
	| b > a  = (searchIDTree right b)
	| b < a  = (searchIDTree left  b)
	| b == a = True

{-
isInIDSearchTree t b 
PURPOSE		: To check if an b is in a t or not, if it is not insert b into t.
PRE 		: True
POST 		: Returns a t with b inserted if b not found in t, else return t. 
EXAMPLES 	: isInIDSearchTree (Branch 10 Empty Empty) 9 = Branch 10 (Branch 9 Empty Empty) Empty
			  isInIDSearchTree (Branch 10 Empty Empty) 10 = Branch 10 Empty Empty
-}
isInIDSearchTree :: Tree Integer -> Integer -> Tree Integer
isInIDSearchTree Empty b = (Branch b Empty Empty)
isInIDSearchTree (Branch a left right) b 
	| (isInIDSearchTreeAux (Branch a left right) b) == True  = (insertIDSearchTree (Branch a left right) b) 
	| (isInIDSearchTreeAux (Branch a left right) b) == False = (Branch a left right)

{-
isInIDSearchTreeAux r b 
PURPOSE		: Check if b is in a t 
PRE 		: True
POST 		: Returns False if b in t.
EXAMPLES 	: isInIDSearchTreeAux (Branch 10 (Branch 9 Empty Empty) Empty) 10 = False
-}
isInIDSearchTreeAux :: Tree Integer -> Integer -> Bool
isInIDSearchTreeAux Empty a = True 
isInIDSearchTreeAux (Branch a left right) b 
	| b == a = False
	| b > a  = (isInIDSearchTreeAux right b)
	| b < a  = (isInIDSearchTreeAux left b)

{-
insertIDSearchTree t b 
PURPOSE		: To insert b to t.
PRE 		: True
POST 		: Returns t with b inserted.
EXAMPLES 	: insertIDSearchTree (Branch 10 Empty Empty) 1 = (Branch 10 (Branch 1 Empty Empty) Empty)		
-}
insertIDSearchTree :: Tree Integer -> Integer -> Tree Integer
insertIDSearchTree Empty b = (Branch b Empty Empty) 
insertIDSearchTree (Branch a left right) b 
	| b > a  = (Branch a left (insertIDSearchTree right b))
	| b < a  = (Branch a (insertIDSearchTree left b) right)

--Test 1; searchTuple, adding value that already excists i the tree.
iDSTtest1 = TestCase (assertEqual "for (searchTuple ((Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty)))) 10)," (Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty)),True) (searchTuple (Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty))) 10))

--Test 2; searchTuple, adding value that is not curently present in the tree.
iDSTtest2 = TestCase (assertEqual "for (searchTuple (Branch 10 Empty Empty) 9)," (Branch 10 (Branch 9 Empty Empty) Empty,False) (searchTuple (Branch 10 Empty Empty) 9))

--Test 3; searchTuple, adding to a empty tree. 
iDSTtest3 = TestCase (assertEqual "for (searchTuple Empty 10," (Branch 10 Empty Empty, False) (searchTuple Empty 10))

--Test 4; searchIDTree, searching a empty tree.
iDSTtest4 = TestCase (assertEqual "for (searchIDTree Empty 10)," False (searchIDTree Empty 10))

--Test 5; searchIDTree, searching a tree with no match.
iDSTtest5 = TestCase (assertEqual "for (searchIDTree (Branch 9 Empty (Branch 11 Empty Empty)) 10)," False (searchIDTree (Branch 9 Empty (Branch 11 Empty Empty)) 10))

--Test 6; searchIDTree, searching a tree and finding a match.
iDSTtest6 = TestCase (assertEqual "for (searchIDTree (Branch 9 Empty (Branch 11 Empty Empty)) 11)," True (searchIDTree (Branch 9 Empty (Branch 11 Empty Empty)) 11))

iDSTtests = TestList [iDSTtest1,iDSTtest2, iDSTtest3, iDSTtest4, iDSTtest5,iDSTtest6]




