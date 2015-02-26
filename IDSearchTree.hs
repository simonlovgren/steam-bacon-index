module IDSearchTree where
{-

-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show)
{-
searchTuple (Tree Integer) Integer (Tree Integer, Bool)
PURPOSE		: To create a tuple with a binary tree of integers and a bool if provided integer was already in the searchtree.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
searchTuple :: Tree Integer -> Integer -> (Tree Integer, Bool)
searchTuple Empty b = ((isInIDSearchTree Empty b),(searchIDTree Empty b))
searchTuple (Branch a left right) b = ((isInIDSearchTree (Branch a left right) b ),(searchIDTree (Branch a left right) b))

{-
searchIDTree (Tree Integer) Integer Bool
PURPOSE		: To return a True is Integer is found in the tree and a False if integer is not found in the tree.
PRE 		: 
POST 		: A boolean
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
isInIDSearchTree (Tree Integer) Integer (Tree Integer)
PURPOSE		: To check if an integer is in a tree or not, if it is, return the tree, else return a tree with integer inserted
PRE 		: 
POST 		: A Tree 
EXAMPLES 	:		
-}
isInIDSearchTree :: Tree Integer -> Integer -> Tree Integer
isInIDSearchTree Empty b = (Branch b Empty Empty)
isInIDSearchTree (Branch a left right) b 
					| (isInIDSearchTreeAux (Branch a left right) b) == True  = (insertIDSearchTree (Branch a left right) b) 
					| (isInIDSearchTreeAux (Branch a left right) b) == False = (Branch a left right)

{-
isInIDSearchTreeAux (Tree Integer) Integer Bool
PURPOSE		: Just to examine if a integer is in a tree or not, returns a Boolean 
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
isInIDSearchTreeAux :: Tree Integer -> Integer -> Bool
isInIDSearchTreeAux Empty a = True 
isInIDSearchTreeAux (Branch a left right) b 
					| b == a = False
					| b > a  = (isInIDSearchTreeAux right b)
					| b < a  = (isInIDSearchTreeAux left b)

{-
insertIDSearchTree (Tree Integer) Integer (Tree Integer)
PURPOSE		: To insert a Integer to a binary tree.
PRE 		: 
POST 		:
EXAMPLES 	:		
-}
insertIDSearchTree :: Tree Integer -> Integer -> Tree Integer
insertIDSearchTree Empty b = (Branch b Empty Empty) 
insertIDSearchTree (Branch a left right) b 
					| b > a  = (Branch a left (insertIDSearchTree right b))
					| b < a  = (Branch a (insertIDSearchTree left b) right)
