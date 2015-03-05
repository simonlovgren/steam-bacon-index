{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson
-}
module KeyVal where 
import Test.HUnit
{-
REPRESENTATION CONVENTION : Represents a keyvalue structure where data is structured as:
							KVStr Key Value or 
							KVInt Key Value
REPRESENTATION INVARIANT  : True
-}
data KeyVal = KVStr String String | KVInt String Integer 
		deriving (Eq, Show)
{-
findKVString list key
PURPOSE		: Check for key in list, if found return corresponding value.
PRE 		: A list of KeyVal and a key. No Key can appear more than once in the list.
POST 		: A Maybe String if the supplied key matches any key in the list.
EXAMPLES 	: findKVString [KVStr "SteamName" "Name"] "SteamName" = Just Name
			  findKVString [KVStr "SteamName" "Name"] "SomethingElse" = Nothing
-}
findKVString :: [KeyVal] -> String -> Maybe String
findKVString [] _ = Nothing
findKVString ((KVStr k s):xs) se = if k == se then Just s else findKVString xs se
findKVString ((KVInt k s):xs) se = findKVString xs se 

{-
findKVInt list key
PURPOSE		: Check for key in list, if found return corresponding value. 
PRE 		: A list of KeyVal and a key. No Key can appear more than once in the list.
POST 		: A Maybe Integer if the supplied key matches any keys in the list.
EXAMPLES 	: findKVString [KVStr "SteamIDInt" 412964] "SteamIDInt" = Just 412964
			  findKVString [KVStr "SteamIDInt" 412964] "SomethingElse" = Nothing
-}
findKVInt :: [KeyVal] -> String -> Maybe Integer
findKVInt [] _ = Nothing
findKVInt ((KVInt k s):xs) se = if k == se then Just s else findKVInt xs se
findKVInt ((KVStr k s):xs) se = findKVInt xs se 






-- Test 1; findKVString, With matching key in list 
test1KV = TestCase (assertEqual "for (findKVString [KVStr SteamName Name, KVInt SteamID 123] SteamName)," (Just "Name") (findKVString [KVStr "SteamName" "Name"] "SteamName"))

-- Test 2; findKVString, With NO matching key in list
test2KV = TestCase (assertEqual "for (findKVString [KVStr SteamLongName LongName, KVStr SteamName Name] SteamShortName)," Nothing (findKVString [KVStr "SteamLongName" "LongName", KVStr "SteamName" "Name"] "SteamShortName"))

-- Test 3; findKVInt, With matching key in list
test3KV = TestCase (assertEqual "for (findKVInt [KVInt SteamID 123, KVStr SteamName Name] SteamID)," (Just 123) (findKVInt [KVInt "SteamID" 123, KVStr "SteamName" "Name"] "SteamID"))

-- Test 4; findKVInt, With NO matching key in list
test4KV = TestCase (assertEqual "for (findKVInt [KVInt SomeNumber 123, KVStr SteamName Name] SteamID)," Nothing (findKVInt [KVInt "SomeNumber" 123, KVStr "SteamName" "Name"] "SteamID"))


tests = TestList [test1KV, test2KV, test3KV, test4KV]






-- Test 5; findKVStr, With multiple matching keys in list
--test5 = TestCase (assertEqual "for (findKVString [KVStr SteamName NameOne, KVStr SteamName NameTwo] SteamName," (Just "NameOne") (findKVString [KVStr "SteamName" "NameOne", KVStr "SteamName" "NameTwo"] "SteamName"))

--Test 6; findKVInt, With multiple matching keys in list
--test6 = TestCase (assertEqual "for (findKVInt [KVInt SteamID 123, KVInt SteamID 456] SteamID," (Just 123) (findKVInt [KVInt "SteamID" 123, KVInt "SteamID" 456] "SteamID"))


