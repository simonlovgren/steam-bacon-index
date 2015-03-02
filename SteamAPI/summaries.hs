{-
    Group 30
    Simon Lövgren
    February 2015

    NOTES:
        * Parsing of JSON is done using the Text.JSON-package
        * The following stackexchange was used to understand how to implement the JSON-package:
          http://stackoverflow.com/questions/17844223/json-parsing-in-haskell

    TODO: Review document, restructure and document
-}

module SteamAPI.Summaries (playersExist) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad
import           Text.JSON

-- Import Steam Request
import qualified SteamAPI.Requests (getPlayerSummaries)

-- import KeyVal
import KeyVal

{-
    REPRESENTATION CONVENTION:
        * Represents the response wrapper used by the steam API

    REPRESENTATION INVARIANT:
        * May only be used within this module due to
          otherwise clashing (automatically created)
-}
data ResponseWrapper =
    ResponseWrapper {
        response :: Players
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents wrapper around player list (sent by steam)

    REPRESENTATION INVARIANT:
        TRUE
-}
data Players =
    Players {
        players :: [Player]
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents summary of a Steam player profile 
    REPRESENTATION INVARIANT:
        TRUE
-}
data Player = 
	Player {
    	steamid :: String,      -- SteamID is returned as string by steam API
    	personaname :: String,
        lastlogoff :: Integer,
        profileurl :: String,
        avatar :: String,
        avatarmedium :: String,
        avatarfull :: String
        -- realname :: String,            -- not allways available, may break code
        -- timecreated :: Integer         -- not allways available, may break code
        -- loccountrycode :: String,      -- not allways available, may break code
        -- locstatecode :: string         -- not allways available, may break code
    } deriving (Show)

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer

instance JSON ResponseWrapper where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        ResponseWrapper        <$>
        obj .: "response"
    readJSON _ = mzero

instance JSON Players where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        Players        <$>
        obj .: "players"
    readJSON _ = mzero

instance JSON Player where
    -- Keep the compiler quiet
    showJSON = undefined

    readJSON (JSObject obj) =
        Player                  <$>
        obj .: "steamid"        <*>
        obj .: "personaname"    <*>
        obj .: "lastlogoff"     <*>
        obj .: "profileurl"     <*>
        obj .: "avatar"         <*>
        obj .: "avatarmedium"   <*>
        obj .: "avatarfull"     -- <*>
        -- obj .: "realname"       <*>  -- not allways available, may break code
        -- obj .: "timecreated"    <*>  -- not allways available, may break code
        -- obj .: "loccountrycode" <*>  -- not allways available, may break code
        -- obj .: "locstatecode"        -- not allways available, may break code

    readJSON _ = mzero

-- TODO: Figure out- and comment (.:) function
(.:) :: (JSON a) => JSObject JSValue -> String -> Result a
(.:) = flip valFromObj

--- Custom parsing of custom data types ---



{-
    extractList parsed

    PURPOSE:
        Extract list of players from response

    PRE:
        TRUE

    POST:
        * Returns list of players without wrappers

    EXAMPLES:
        -- Data for testing
        let resp = Ok (ResponseWrapper {response = Players {players = [Player {steamid = "76561197960265731", personaname = "ErikJ", lastlogoff = 1425035719, profileurl = "http://steamcommunity.com/id/erikj/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084_full.jpg"}]}})
        
        extractList resp = [Player {steamid = "76561197960265731", personaname = "ErikJ", lastlogoff = 1425035719, profileurl = "http://steamcommunity.com/id/erikj/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/16/168eb0f2a603cef40869abedb970a7c58daec084_full.jpg"}]


-}
extractList :: Result ResponseWrapper -> [Player]
extractList (Ok x)  = players (response x)
extractList (Error _)  = []



{-
    playersExist list

    PURPOSE:
        Verify all ID:s in list belongs to a steam user

    PRE:
        A maximum of 100 ID:s in supplied list

    POST:
        * Returns True if all exist, False otherwise

    EXAMPLES:
        playersExist [9999999,123456789] == False
        playersExist [76561197979971024] == True

-}
playersExist :: [SteamID] -> IO Bool
playersExist [] = do
    putStrLn "No ID:s supplied to playersExist"
    return $ False

playersExist ids = do
    if ((length ids) > 100)
        then do
            putStrLn "Cannot check more than 100 steam users ath the same time."
            return $ False
    else do
        raw <- SteamAPI.Requests.getPlayerSummaries ids
        -- putStrLn raw
        let parsed = decode raw :: Result ResponseWrapper
        -- print parsed
        return $ (length ids) == (length (extractList parsed))





---- Testdata ----

aa :: String
aa = "{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t}"

bb :: String
bb = "{\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198000124224\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1336242822\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198043343260\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1419003931\n\t\t\t}\n\t\t]\n\t\t\n\t}"

cc :: String
cc = "{\n\t\"friendslist\": {\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197989194839\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1303555871\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198000124224\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1336242822\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198043343260\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1419003931\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"

-- manualTest1 = print (decode aa :: Result Friend)
-- manualTest2 = print (decode bb :: Result Friends)
-- manualTest3 = print (decode cc :: Result FriendsList)


-- TODO: add test cases









{-

RAW RESPONSE IN JSON::

{
    "response": {
        "players": [
            {
                "steamid": "76561197960435530",
                "communityvisibilitystate": 3,
                "profilestate": 1,
                "personaname": "Robin",
                "lastlogoff": 1424949328,
                "profileurl": "http://steamcommunity.com/id/robinwalker/",
                "avatar": "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/f1/f1dd60a188883caf82d0cbfccfe6aba0af1732d4.jpg",
                "avatarmedium": "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/f1/f1dd60a188883caf82d0cbfccfe6aba0af1732d4_medium.jpg",
                "avatarfull": "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/f1/f1dd60a188883caf82d0cbfccfe6aba0af1732d4_full.jpg",
                "personastate": 0,
                "realname": "Robin Walker",
                "primaryclanid": "103582791429521412",
                "timecreated": 1063407589,
                "personastateflags": 0,
                "loccountrycode": "US",
                "locstatecode": "WA",
                "loccityid": 3961
            }
        ]
        
    }
}
-}