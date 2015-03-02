module KeyVal where 
{-
REPRESENTATION CONVENTION : Represents a keyvalue structure where data is structured as:
							KVStr Key Value or 
							KVInt Key Value
REPRESENTATION INVARIANT  : True
-}
data KeyVal = KVStr String String | KVInt String Integer 
		deriving (Eq, Show)
{-
PURPOSE		:
PRE 		:
POST 		:
EXAMPLES 	:
-}
findKVString :: [KeyVal] -> String -> Maybe String
findKVString [] _ = Nothing
findKVString ((KVStr k s):xs) se = if k == se then Just s else findKVString xs se
findKVString ((KVInt k s):xs) se = findKVString xs se 

{-
PURPOSE		:
PRE 		:
POST 		:
EXAMPLES 	:
-}
findKVInt :: [KeyVal] -> String -> Maybe Integer
findKVInt [] _ = Nothing
findKVInt ((KVInt k s):xs) se = if k == se then Just s else findKVInt xs se
findKVInt ((KVStr k s):xs) se = findKVInt xs se 







{-test1 = TestCase (assertEqual "for (searchTuple ((Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty)))) 10)," (Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty)),True) (searchTuple (Branch 10 (Branch 9 Empty Empty) (Branch 11 Empty (Branch 12 Empty Empty))) 10))
test2 = TestCase (assertEqual "for (searchTuple (Branch 10 Empty Empty) 10)," (Branch 10 Empty Empty,True) (searchTuple (Branch 10 Empty Empty) 10))

tests = TestList [TestLabel "Lite svår" test1, TestLabel "Lätt" test2]-}