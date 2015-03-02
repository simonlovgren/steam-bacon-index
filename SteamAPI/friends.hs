{-  
    Group 30
    Simon Lövgren
    February 2015

    NOTES:
        * Parsing of JSON is done using the Text.JSON-package
        * The following stackexchange was used to understand how to implement the JSON-package:
          http://stackoverflow.com/questions/17844223/json-parsing-in-haskell

    TODO: Review document
-}

module SteamAPI.Friends (getIDList, getRawList) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad
import           Text.JSON

import qualified SteamAPI.Requests (getFriendList)
import KeyVal

{-
    REPRESENTATION CONVENTION:
        * Represents the response wrapper used by the steam API

    REPRESENTATION INVARIANT:
        * May only be used within this module due to
          otherwise clashing (automatically created)
-}
data FriendsList =
    FriendsList {
        friendslist :: Friends
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents the list of friends in friend request

    REPRESENTATION INVARIANT:
        TRUE
-}
data Friends =
    Friends {
        friends :: [Friend]
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents a friend in the steam friend list, where
          steamid is the steam ID of tehe friend
          relationship is what type of relationship is between user and friend
          friend_since is integer (unix tiemstamp) of when user became friends with friend

    REPRESENTATION INVARIANT:
        TRUE
-}
data Friend = 
	Friend {
    	steamid :: String,      -- SteamID is returned as string by steam API
    	relationship :: String,
    	friend_since :: Integer
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer

instance JSON FriendsList where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        FriendsList        <$>
        obj .: "friendslist"
    readJSON _ = mzero

instance JSON Friends where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        Friends        <$>
        obj .: "friends"
    readJSON _ = mzero

instance JSON Friend where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        Friend       <$>
        obj .: "steamid"        <*>
        obj .: "relationship"   <*>
        obj .: "friend_since"

    readJSON _ = mzero

-- TODO: Figure out- and comment (.:) function
(.:) :: (JSON a) => JSObject JSValue -> String -> Result a
(.:) = flip valFromObj

--- Custom parsing of custom data types ---
{-
    makeListOfIDs friendslist

    PURPOSE:
        Extract steam ID:s from friendslist into a list

    PRE:
        TRUE

    POST:
        * Returns list of integers with steam ID:s of friends in friendslist 

    EXAMPLES:
        makeListOfIDs Error _ = []
        makeListOfIDs Ok (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})}) = [76561197965528292]

-}
makeListOfIDs :: Result FriendsList -> [SteamID]
makeListOfIDs (Ok x) = [read (steamid x) | x <- (friends (friendslist(x)))]
makeListOfIDs (Error _) = []

{-
    extractFriends friendslist

    PURPOSE:
        Extract raw list of friends

    PRE:
        TRUE

    POST:
        * Returns list of Friend data type with steam ID:s of friends in friendslist. If error or nothing found- returns empty list.

    EXAMPLES:
        extractFriends Nothing = []
        extractFriends Just (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})}) = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]
-}
extractFriends :: Result FriendsList -> [[KeyVal]]
extractFriends (Ok x) =
    let
        extractFriends' :: [Friend] -> [[KeyVal]]
        extractFriends' [] = []
        extractFriends' (friend:xs) = [KVInt "steamid" (read (steamid friend)), KVStr "relationship" (relationship friend), KVInt "friend_since" (friend_since friend)] : extractFriends' xs
    in
        extractFriends' (friends (friendslist(x)))
extractFriends (Error _) = []


{-
    getIDs id

    PURPOSE:
        Return list of steam id:s of friends to user of supplied id

    PRE:
        TRUE

    POST:
        * Returns list of integers with steam ID:s of friends in friendslist 

    EXAMPLES:
        ---

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
        Return Friends data type

    PRE:
        TRUE

    POST:
        * Returns Maybe [Friend], if something went wrong returns Nothing

    EXAMPLES:
        ---

-}
getRawList :: SteamID -> IO [[KeyVal]]
getRawList id = do
    raw <- SteamAPI.Requests.getFriendList id
    -- putStrLn raw
    let parsed = decode raw :: Result FriendsList
    return $ extractFriends parsed



---- Testdata ----

aa :: String
aa = "{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t}"

bb :: String
bb = "{\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198000124224\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1336242822\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198043343260\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1419003931\n\t\t\t}\n\t\t]\n\t\t\n\t}"

cc :: String
cc = "{\n\t\"friendslist\": {\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198000124224\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1336242822\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198043343260\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1419003931\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"

manualTest1 = print (decode aa :: Result Friend)
manualTest2 = print (decode bb :: Result Friends)
manualTest3 = print (decode cc :: Result FriendsList)


-- TODO: add test cases