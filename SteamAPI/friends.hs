{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson

    NOTES:
        * Parsing of JSON is done using the Text.JSON-package
        * The following stackexchange was used to understand how to implement the JSON-package:
          http://stackoverflow.com/questions/17844223/json-parsing-in-haskell
-}

module SteamAPI.Friends (getIDList, getRawList, steamFriendsTests) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad
import           Text.JSON

import qualified SteamAPI.Requests (getFriendList)
import KeyVal

import Test.HUnit

{-
    REPRESENTATION CONVENTION:
        * Represents the response wrapper used by the steam API

    REPRESENTATION INVARIANT:
        * May only be used within this module due to
          otherwise clashing (automatically created)
          functions
-}
data FriendsList =
    FriendsList {
        friendslist :: Friends
    } deriving (Show, Eq)

{-
    REPRESENTATION CONVENTION:
        * Represents the list of friends in friend request

    REPRESENTATION INVARIANT:
        * May only be used within this module due to
          otherwise clashing (automatically created)
          functions
-}
data Friends =
    Friends {
        friends :: [Friend]
    } deriving (Show, Eq)

{-
    REPRESENTATION CONVENTION:
        * Represents a friend in the steam friend list, where
          steamid is the steam ID of the friend
          relationship is what type of relationship is between user and friend
          friend_since is integer (unix tiemstamp) of when user became friends with friend

    REPRESENTATION INVARIANT:
        * May only be used within this module due to
          otherwise clashing (automatically created)
          functions
-}
data Friend = 
	Friend {
    	steamid :: String,      -- SteamID is returned as string by steam API
    	relationship :: String,
    	friend_since :: Integer
    } deriving (Show, Eq)

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer

instance JSON FriendsList where
    {-
        NOTE:
            * Unused function, but needs to be
              declared as to be able to compile.
    -}
    showJSON = undefined

    {-
        NOTE:
            * Instance created in accordance with Text.JSON library
              with help of stackexchange post (see note at top of 
              document).
    -}
    readJSON (JSObject obj) =
        FriendsList        <$>
        obj .: "friendslist"
    readJSON _ = mzero

instance JSON Friends where
    {-
        NOTE:
            * Unused function, but needs to be
              declared as to be able to compile.
    -}
    showJSON = undefined

    {-
        NOTE:
            * Instance created in accordance with Text.JSON library
              with help of stackexchange post (see note at top of 
              document).
    -}
    readJSON (JSObject obj) =
        Friends        <$>
        obj .: "friends"
    readJSON _ = mzero

instance JSON Friend where
    {-
        NOTE:
            * Unused function, but needs to be
              declared as to be able to compile.
    -}
    showJSON = undefined

    {-
        NOTE:
            * Instance created in accordance with Text.JSON library
              with help of stackexchange post (see note at top of 
              document).
    -}
    readJSON (JSObject obj) =
        Friend       <$>
        obj .: "steamid"        <*>
        obj .: "relationship"   <*>
        obj .: "friend_since"

    readJSON _ = mzero

{-
    obj .: field
    
    PURPOSE:
        Simplify chaining of applicatives for parsing json to custom data type

    PRE:
        TRUE

    POST:
        N/A

    NOTE:
        This function was created according to the stackexchange post used
        to understand the implementation of the JSON library (see note at top
        of document). Unfortunately I'm not fully aware of why there's a need
        to flip the valFromObj and its two inputs, only that it's needed for
        chaining using the applicative library.
-}
(.:) :: (JSON a) => JSObject JSValue -> String -> Result a
(.:) = flip valFromObj


{-
    makeListOfIDs friendslist

    PURPOSE:
        Extract steam ID:s from friendslist into a list

    PRE:
        TRUE

    POST:
        Returns list of integers with steam ID:s of friends in friendslist 

    EXAMPLES:
        makeListOfIDs (Error _) = []
        makeListOfIDs (Ok (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})})) = [76561197965528292]

-}
makeListOfIDs :: Result FriendsList -> [SteamID]
makeListOfIDs (Ok x) = [read (steamid x) | x <- (friends (friendslist(x)))]
makeListOfIDs (Error _) = []

{-
    extractFriends friendslist

    PURPOSE:
        Extract list of friends as list of KeyVal-lists

    PRE:
        TRUE

    POST:
        Returns list of KeyVal-lists with steamid, relationship and friend_since as data. If error or nothing found- returns empty list.

    EXAMPLES:
        extractFriends (Error _) = []
        extractFriends (Ok (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})})) = [[KVInt "steamid" 76561197965528292, KVStr "relationship" "friend", KVInt "friend_since" 1223307109]]
-}
extractFriends :: Result FriendsList -> [[KeyVal]]
extractFriends (Ok x) =
    let
        {-
            extractFriends' list

            PURPOSE:
                Convert list of friends to list of KeyVal-lists 

            PRE:
                TRUE

            POST:
                Returns list of KeyVal-lists with steamid, relationship and friend_since as data. If error or nothing found- returns empty list.

            EXAMPLES:
                extractFriends' [] = []
                extractFriends' [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}] = [[KVInt "steamid" 76561197965528292, KVStr "relationship" "friend", KVInt "friend_since" 1223307109]]
        -}
        extractFriends' :: [Friend] -> [[KeyVal]]
        extractFriends' [] = []
        extractFriends' (friend:xs) = [KVInt "steamid" (read (steamid friend)), KVStr "relationship" (relationship friend), KVInt "friend_since" (friend_since friend)] : extractFriends' xs
    in
        extractFriends' (friends (friendslist(x)))
extractFriends (Error _) = []


{-
    getIDs id

    PURPOSE:
        Fetch list of steam id:s of friends to user of supplied id

    PRE:
        TRUE

    POST:
        Returns list of integers with steam ID:s of friends in friendslist

    EXAMPLES:
        getIDList 76561198028357851 = IO [76561197979971024,76561197989194839,76561198000124224,76561198043343260]

-}
getIDList :: SteamID -> IO [Integer]
getIDList id = do
    raw <- SteamAPI.Requests.getFriendList id
    -- putStrLn raw
    let parsed = decode raw :: Result FriendsList
    return $ makeListOfIDs parsed

{-
    getRawList id

    PURPOSE:
        Fetch list of friends as list of KeyVal-lists

    PRE:
        TRUE

    POST:
        Returns IO [[KeyVal]], if something went wrong returns []

    EXAMPLES:
       getRawList 76561198028357851 = IO [[KVInt "steamid" 76561197979971024,KVStr "relationship" "friend",KVInt "friend_since" 1424960766],[KVInt "steamid" 76561197989194839,KVStr "relationship" "friend",KVInt "friend_since" 1303555871],[KVInt "steamid" 76561198000124224,KVStr "relationship" "friend",KVInt "friend_since" 1336242822],[KVInt "steamid" 76561198043343260,KVStr "relationship" "friend",KVInt "friend_since" 1419003931]]

-}
getRawList :: SteamID -> IO [[KeyVal]]
getRawList id = do
    raw <- SteamAPI.Requests.getFriendList id
    -- putStrLn raw
    let parsed = decode raw :: Result FriendsList
    return $ extractFriends parsed


{-
    Test cases
-}

{-
    testJSONData

    PURPOSE:
        Fetch JSON formatted data for test purposes

    PRE:
        TRUE

    POST:
        Returns JSON data in form of a IO String in same form as a real API call using ID: 76561197979971024, though limited to 4 friends for testing purposes
-}
testJSONData :: String
testJSONData = "{\n\t\"friendslist\": {\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197963805864\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197963864986\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197964446578\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288114261\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197965528292\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1223307109}\n\t\t]\n\t\t\n\t}\n}"
{-
    testData

    PURPOSE:
        Fetch Result wrapped parsed test data

    PRE:
        TRUE

    POST:
        Returns parsed test data as the parsing would have created, in form of Result FriendsList
-}
testParsedData :: Result FriendsList
testParsedData = (Ok (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197963805864", relationship = "friend", friend_since = 0}, Friend {steamid = "76561197963864986", relationship = "friend", friend_since = 0}, Friend {steamid = "76561197964446578", relationship = "friend", friend_since = 1288114261}, Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})}))

-- Test 1; Test parsing of JSON
test1 = TestCase $ assertEqual "Parsing of JSON data" testParsedData (decode testJSONData :: Result FriendsList)
-- Test 2; Extracting list of ID:s from parsed data
test2 = TestCase $ assertEqual "Extracting list of ID:s only" [76561197963805864,76561197963864986,76561197964446578,76561197965528292] (makeListOfIDs testParsedData)
-- Test 3; Extracting list of ID:s from failed parsing
test3 = TestCase $ assertEqual "Extracting list of ID:s - Failed parsing data" [] (makeListOfIDs (Error "Failed parsing data"))
-- Test 4; Extract list of friends in KeyVal-list format
test4 = TestCase $ assertEqual "Extracting friends to list of KeyVal-lists" [[KVInt "steamid" 76561197963805864, KVStr "relationship" "friend", KVInt "friend_since" 0],[KVInt "steamid" 76561197963864986, KVStr "relationship" "friend", KVInt "friend_since" 0],[KVInt "steamid" 76561197964446578, KVStr "relationship" "friend", KVInt "friend_since" 1288114261],[KVInt "steamid" 76561197965528292, KVStr "relationship" "friend", KVInt "friend_since" 1223307109]] (extractFriends testParsedData) 
-- Test 5; Extract list of friends in KeyVal-list format from failed parsing
test5 = TestCase $ assertEqual "Extracting friends to list of KeyVal-lists - failed parsing data" [] (extractFriends (Error "Failed parsing data")) 

steamFriendsTests = TestList [test1,test2,test3,test4,test5]
