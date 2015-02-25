{-# LANGUAGE OverloadedStrings #-}
{-  
    Group 30
    Simon Lövgren
    February 2015

    NOTES:
        * Parsing of JSON is done using the AESON-package
        * The following blog post was used to understand how to implement the Aeson-package:
          http://blog.raynes.me/blog/2012/11/27/easy-json-parsing-in-haskell-with-aeson/
-}

module SteamFriends (getIDs, SteamID) where

import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Data.Aeson ((.:),(.:?),decode,FromJSON(..), Value(..))
import qualified SteamAPI (getFriendList)
import qualified Data.ByteString.Lazy.Char8 as BS


{-
    REPRESENTATION CONVENTION:
        * Represents the response wrapper used by the steam API

    REPRESENTATION INVARIANT:
        TRUE
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
    EXAMPLE:
        Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}
-}

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
-}
type SteamID = Integer

{-
    Instance of FromJSON for datatype Friend
-}
instance FromJSON Friend where
    parseJSON (Object v) =
        Friend <$>
        (v .: "steamid")        <*>
        (v .: "relationship")   <*>
        (v .: "friend_since")

    parseJSON _ = mzero

{-
    Instance of FromJSON for datatype Friends
-}
instance FromJSON Friends where
    parseJSON (Object v) =
        Friends <$>
        (v .: "friends")

    parseJSON _ = mzero

{-
    Instance of FromJSON for datatype FriendsList
-}
instance FromJSON FriendsList where
    parseJSON (Object v) =
        FriendsList <$>
        (v .: "friendslist")

    parseJSON _ = mzero


{-
    makeListOfIDs friendslist

    PURPOSE:
        Extract steam ID:s from friendslist into a list

    PRE:
        TRUE

    POST:
        * Returns list of integers with steam ID:s of friends in friendslist 

    EXAMPLES:
        makeListOfIDs Nothing = []
        makeListOfIDs Just (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})}) = [76561197965528292]

-}
makeListOfIDs :: Maybe FriendsList -> [SteamID]
makeListOfIDs (Just x) = [read (steamid x) | x <- (friends (friendslist(x)))]
makeListOfIDs Nothing = []


{-
    getIDs id

    PURPOSE:
        Return list of steam id:s of friends to user of supplied id

    PRE:
        TRUE

    POST:
        * Returns list of integers with steam ID:s of friends in friendslist 

    EXAMPLES:
        makeListOfIDs Nothing = []
        makeListOfIDs Just (FriendsList {friendslist = (Friends {friends = [Friend {steamid = "76561197965528292", relationship = "friend", friend_since = 1223307109}]})}) = [76561197965528292]

-}
getIDs :: SteamID -> IO [SteamID]
getIDs id = do
    raw <- SteamAPI.getFriendList id
    -- putStrLn raw
    let parsed = decode $ BS.pack raw :: Maybe FriendsList
    return $ makeListOfIDs parsed



--- Test data ---
jsonTestFriendlist :: String
jsonTestFriendlist = "{\n\t\"friendslist\": {\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198000124224\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1336242822\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198043343260\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1419003931\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"

jsonTestFriend :: String
jsonTestFriend = "{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t}"

{-
    fromMaybe value

    PURPOSE:
        Extract data from Maybe wrapper (used in manual tests only)

    PRE:
        TRUE

    POST:
        * Returns data unwrapped from Maybe data type

    EXAMPLES:
        fromMaybe (Just 3) = 3
        fromMaybe Nothing = *** Exception: Nothing supplied

-}
fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a
fromMaybe Nothing = error "Nothing supplied"

{-
extractList :: String -> String
extractList (x@'[':xs) =
    let
        extractList' (x@']':_) = x : []
        extractList' (x:xs) = x : extractList' xs
    in
        x : extractList' xs
extractList (_:xs) = extractList xs

stripSpecials :: String -> String
stripSpecials [] = []
stripSpecials (x:xs)
    | x == '\t' || x == '\n' = stripSpecials xs
    | otherwise = x:(stripSpecials xs)
-}

{-
    TODO: Add test cases
-}
