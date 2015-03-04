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

module SteamAPI.Summaries (playersExist, getPlayerList, getOrderedNames, steamSummariesTests) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad
import           Text.JSON

import qualified SteamAPI.Requests (getPlayerSummaries)
import KeyVal

import Test.HUnit

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
    } deriving (Show, Eq)

{-
    REPRESENTATION CONVENTION:
        * Represents wrapper around player list (sent by steam)

    REPRESENTATION INVARIANT:
        TRUE
-}
data Players =
    Players {
        players :: [Player]
    } deriving (Show, Eq)

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
    } deriving (Show, Eq)

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID

    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer

instance JSON ResponseWrapper where
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
        ResponseWrapper        <$>
        obj .: "response"
    readJSON _ = mzero

instance JSON Players where
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
        Players        <$>
        obj .: "players"
    readJSON _ = mzero

instance JSON Player where
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
        Player                  <$>
        obj .: "steamid"        <*>
        obj .: "personaname"    <*>
        obj .: "lastlogoff"     <*>
        obj .: "profileurl"     <*>
        obj .: "avatar"         <*>
        obj .: "avatarmedium"   <*>
        obj .: "avatarfull"

    readJSON _ = mzero

{-
    obj .: field
    
    PURPOSE:
        Simplify chaining of applicatives for parsing json to custom data type

    PRE:
        TRUE

    POST:
        * ???

    NOTE:
        * This function was created according to the stackexchange post used
          to understand the implementation of the JSON library (see note at top
          of document). Unfortunately I'm not fully aware of why there's a need
          to flip the valFromObj and its two inputs, only that it's needed for
          chaining using the applicative library.
-}
(.:) :: (JSON a) => JSObject JSValue -> String -> Result a
(.:) = flip valFromObj

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

    SIDEEFFECTS:
        * Uses SteamAPI.Requests module to fetch external data 

    EXAMPLES:
        playersExist [9999999,123456789] = False
        playersExist [76561197979971024] = True

-}
playersExist :: [SteamID] -> IO Bool
playersExist [] = do
    putStrLn "No ID:s supplied to playersExist"
    return $ False

playersExist ids = do
    if ((length ids) > 100)
        then do
            putStrLn "Cannot check more than 100 steam users at the same time."
            return $ False
    else do
        raw <- SteamAPI.Requests.getPlayerSummaries ids
        let parsed = decode raw :: Result ResponseWrapper

        return $ (length ids) == (length (extractList parsed))



{-
    playerList playerlist

    PURPOSE:
        Extract raw list of players to KeyVal-list

    PRE:
        TRUE

    POST:
        * Returns list of KeyVal type with player data, empty list on error or no result

    EXAMPLES:
        -- example data
        let data = (Ok (ResponseWrapper {response = (Players {players = [Player {steamid = "76561197979971024", personaname = "Maustronaut", lastlogoff = 1425457171, profileurl = "http://steamcommunity.com/profiles/76561197979971024/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"},Player {steamid = "76561197999847293", personaname = "Avari", lastlogoff = 1425417881, profileurl = "http://steamcommunity.com/profiles/76561197999847293/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"}]})}))
        playerList data == [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"],[KVInt "steamid" 76561197999847293,KVStr "personaname" "Avari",KVInt "lastlogoff" 1425417881,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197999847293/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"]]
        playerList (Error _) == []
 
-}
playerList :: Result ResponseWrapper -> [[KeyVal]]
playerList l@(Ok x) =
    let
        playerList' :: [Player] -> [[KeyVal]]
        playerList' [] = []
        playerList' (player:xs) = [KVInt "steamid" (read (steamid player)), KVStr "personaname" (personaname player), KVInt "lastlogoff" (lastlogoff player), KVStr "profileurl" (profileurl player), KVStr "avatar" (avatar player), KVStr "avatarmedium" (avatarmedium player), KVStr "avatarfull" (avatarfull player)] : playerList' xs
    in
        playerList' (extractList l)
playerList (Error _) = []


{-
    orderedNames result list

    PURPOSE:
        * Return player personanames for supplied ID:s in same order as ID:s 

    PRE:
        * True

    POST:
        * Returns list of Maybe String. Just "personaname" if personaname was found, otherwise Nothing

    EXAMPLES:
        -- example data
        let data = (Ok (ResponseWrapper {response = (Players {players = [Player {steamid = "76561197979971024", personaname = "Maustronaut", lastlogoff = 1425457171, profileurl = "http://steamcommunity.com/profiles/76561197979971024/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"},Player {steamid = "76561197999847293", personaname = "Avari", lastlogoff = 1425417881, profileurl = "http://steamcommunity.com/profiles/76561197999847293/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"}]})}))

        orderedNames data [76561197979971024,76561197999847293] == [Just "Maustronaut", Just "Avari"]
        orderedNames data [76561197999847293,76561197979971024] == [Just "Avari",Just "Maustronaut"]
        orderedNames data [76561197999847293] == [Just "Avari"]

        orderedNames data [76561197999847293,76561197999847293,76561197999847293,76561197999847293] == [Just "Avari",Just "Avari",Just "Avari",Just "Avari"]
        orderedNames data [76561197847293, 99999999999] = [Nothing, Nothing]
        orderedNames data [] == []

-}
orderedNames :: Result ResponseWrapper -> [SteamID] -> [Maybe String]
orderedNames l@(Ok _) ids =
    let
        {-
            orderedNames' ids summaries

            PURPOSE:
                * Return player personanames for supplied ID:s in same order as ID:s 

            PRE:
                * True

            POST:
                * Returns list of Maybe String. Just "personaname" if personaname was found, otherwise Nothing

            EXAMPLES:
                -- example data
                let data = [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"],[KVInt "steamid" 76561197999847293,KVStr "personaname" "Avari",KVInt "lastlogoff" 1425417881,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197999847293/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"]]
                
                orderedNames' [76561197979971024,76561197999847293] data == [Just "Maustronaut", Just "Avari"]
                orderedNames' [76561197999847293,76561197979971024] data == [Just "Avari",Just "Maustronaut"]
                orderedNames' [76561197999847293] data == [Just "Avari"]

                orderedNames' [76561197999847293,76561197999847293,76561197999847293,76561197999847293] data == [Just "Avari",Just "Avari",Just "Avari",Just "Avari"]
                orderedNames' [76561197847293, 99999999999] data = [Nothing, Nothing]
                orderedNames' [] data == []

        -}
        orderedNames' :: [SteamID] -> [[KeyVal]] -> [Maybe String]
        orderedNames' [] _ = []
        orderedNames' (id:ids) players = (nameOfPlayer id players) : orderedNames' ids players
    in
        orderedNames' ids (playerList l)

orderedNames (Error _) _ = []



{-
    nameOfPlayer id keyval

    PURPOSE:
        * Return personaname of player with Steam ID

    PRE:
        * True

    POST:
        * Returns Maybe String. Just "personaname" if persona name linked to id was found in supplied keyval set, otherwise Nothing

    EXAMPLES:
        -- example data
        let data = [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"],[KVInt "steamid" 76561197999847293,KVStr "personaname" "Avari",KVInt "lastlogoff" 1425417881,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197999847293/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"]]
        nameOfPlayer 76561197979971024 data == Just "Maustronaut"
        nameOfPlayer 76561197888871024 data == Nothing

-}
nameOfPlayer :: SteamID -> [[KeyVal]] -> Maybe String
nameOfPlayer id (player:players)
    | (findKVInt player "steamid") == (Just id) = findKVString player "personaname"
    | otherwise = nameOfPlayer id players
nameOfPlayer id [] = Nothing


{-
    getPlayerList list

    PURPOSE:
        * Return player summaries for supplied ID:s 

    PRE:
        * A minimum of 1 SteamID in list
        * A maximum of 100 SteamID:s in list

    POST:
        * Returns IO list of KeyVal-lists containing steam player summaries

    SIDEEFFECTS:
        * Uses SteamAPI.Requests module to fetch external data
        * Steam accounts with privacy other than public will not be returned in list

    EXAMPLES:
        getPlayerList [76561197979971024] = IO [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"]]
        
        getPlayerList [] = [] and prints an error message
-}
getPlayerList :: [SteamID] -> IO ([[KeyVal]])
getPlayerList [] = do
    putStrLn "No ID:s supplied to getPlayerList"
    return $ []

getPlayerList ids = do
    if ((length ids) > 100)
        then do
            putStrLn "Cannot check more than 100 steam users at the same time."
            return $ []
    else do
        raw <- SteamAPI.Requests.getPlayerSummaries ids
        let parsed = decode raw :: Result ResponseWrapper
        
        return $ playerList parsed



{-
    getOrderedNames list

    PURPOSE:
        * Return player personanames for supplied ID:s in same order as ID:s 

    PRE:
        * A minimum of 1 SteamID in list
        * A maximum of 100 SteamID:s in list

    POST:
        * Returns IO list of Maybe String. Just "personaname" if personaname was found, otherwise Nothing

    SIDEEFFECTS:
        * Uses SteamAPI.Requests module to fetch external data
        * Steam accounts with privacy other than public will not be returned in list

    EXAMPLES:
        playerNamesOrdered [76561198028357851,76561197979971024,76561197999847293] = IO [Just "Erikun", Just "Maustronaut", Just "Avari"]
        playerNamesOrdered [76561197999847293, 76561197979971024, 76561198028357851] = IO [Just "Avari", Just "Maustronaut", Just "Erikun"]

        playerNamesOrdered [76561197999847293, 55666, 76561198028357851] = IO [Just "Avari", Nothing, Just "Erikun"]
        playerNamesOrdered [] = IO [] and prints an error message

-}
getOrderedNames :: [SteamID] -> IO ([Maybe String])
getOrderedNames [] = do
    putStrLn "No ID:s supplied to getOrderedNames"
    return $ []

getOrderedNames ids = do
    if ((length ids) > 100)
        then do
            putStrLn "Cannot check more than 100 steam users at the same time."
            return $ []
    else do
        raw <- SteamAPI.Requests.getPlayerSummaries ids
        let parsed = decode raw :: Result ResponseWrapper

        return $ orderedNames parsed ids




{-
    Test cases
-}

{-
    testJSONData

    PURPOSE:
        Return JSON formatted data for test purposes

    PRE:
        * TRUE

    POST:
        * Returns test data in form of a IO String in same form as a real API call using ID: 76561197979971024
-}
testJSONData :: String
testJSONData = "{\n\t\"response\": {\n\t\t\"players\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197979971024\",\n\t\t\t\t\"communityvisibilitystate\": 3,\n\t\t\t\t\"profilestate\": 1,\n\t\t\t\t\"personaname\": \"Maustronaut\",\n\t\t\t\t\"lastlogoff\": 1425457171,\n\t\t\t\t\"profileurl\": \"http://steamcommunity.com/profiles/76561197979971024/\",\n\t\t\t\t\"avatar\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg\",\n\t\t\t\t\"avatarmedium\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg\",\n\t\t\t\t\"avatarfull\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg\",\n\t\t\t\t\"personastate\": 0,\n\t\t\t\t\"realname\": \"Simon Lövgren\",\n\t\t\t\t\"primaryclanid\": \"103582791431467770\",\n\t\t\t\t\"timecreated\": 1135541046,\n\t\t\t\t\"personastateflags\": 0,\n\t\t\t\t\"loccountrycode\": \"SE\",\n\t\t\t\t\"locstatecode\": \"21\",\n\t\t\t\t\"loccityid\": 43767\n\t\t\t},\n\t\t\t\t{\n\t\t\t\t\"steamid\": \"76561197999847293\",\n\t\t\t\t\"communityvisibilitystate\": 3,\n\t\t\t\t\"profilestate\": 1,\n\t\t\t\t\"personaname\": \"Avari\",\n\t\t\t\t\"lastlogoff\": 1425417881,\n\t\t\t\t\"profileurl\": \"http://steamcommunity.com/profiles/76561197999847293/\",\n\t\t\t\t\"avatar\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg\",\n\t\t\t\t\"avatarmedium\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg\",\n\t\t\t\t\"avatarfull\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg\",\n\t\t\t\t\"personastate\": 1,\n\t\t\t\t\"primaryclanid\": \"103582791430380703\",\n\t\t\t\t\"timecreated\": 1215871590,\n\t\t\t\t\"personastateflags\": 0,\n\t\t\t\t\"loccountrycode\": \"SE\",\n\t\t\t\t\"locstatecode\": \"21\",\n\t\t\t\t\"loccityid\": 43767\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"
{-
    testParsedData

    PURPOSE:
        Return Result wrapped parsed test data

    PRE:
        * TRUE

    POST:
        * Returns test data as the parsing would have created, in form of Result ResponseWrapper
-}
testParsedData :: Result ResponseWrapper
testParsedData = (Ok (ResponseWrapper {response = (Players {players = [Player {steamid = "76561197979971024", personaname = "Maustronaut", lastlogoff = 1425457171, profileurl = "http://steamcommunity.com/profiles/76561197979971024/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"},Player {steamid = "76561197999847293", personaname = "Avari", lastlogoff = 1425417881, profileurl = "http://steamcommunity.com/profiles/76561197999847293/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"}]})}))
{-
    testKeyValData

    PURPOSE:
        Return player summaries for testing purposes

    PRE:
        * TRUE

    POST:
        * Returns test data as the playerList would have returned it, as [[KeyVal]]
-}
testKeyValData :: [[KeyVal]]
testKeyValData = [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"],[KVInt "steamid" 76561197999847293,KVStr "personaname" "Avari",KVInt "lastlogoff" 1425417881,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197999847293/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"]]



-- Test 1; Test parsing of JSON
test1 = TestCase $ assertEqual "Parsing of JSON test data" testParsedData (decode testJSONData :: Result ResponseWrapper)
-- Test 2; Extract raw list of players ([Player])
test2 = TestCase $ assertEqual "Extract raw list of players" ([Player {steamid = "76561197979971024", personaname = "Maustronaut", lastlogoff = 1425457171, profileurl = "http://steamcommunity.com/profiles/76561197979971024/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"},Player {steamid = "76561197999847293", personaname = "Avari", lastlogoff = 1425417881, profileurl = "http://steamcommunity.com/profiles/76561197999847293/", avatar = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", avatarmedium = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg", avatarfull = "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"}]) (extractList testParsedData)
-- Test 3; Extract raw list of players ([Player]) from failed parsing
test3 = TestCase $ assertEqual "Extract list from failed parsing" [] (extractList (Error "Failed parsing data"))
-- Test 4; Extract list of player summaries to a list of KeyVal-lists
test4 = TestCase $ assertEqual "Extract summaries to KeyVal" [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"],[KVInt "steamid" 76561197999847293,KVStr "personaname" "Avari",KVInt "lastlogoff" 1425417881,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197999847293/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg", KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg"]]  (playerList testParsedData)
-- Test 5; Extract list of player summaries to a list of KeyVal-lists with failed parsing
test5 = TestCase $ assertEqual "Extract summaries to KeyVal - failed parsing" [] (playerList (Error "Failed parsing data"))
-- Test 6; Extract name of specific player summary based on ID
test6 = TestCase $ assertEqual "Extract specific name" (Just "Maustronaut") (nameOfPlayer 76561197979971024 testKeyValData)
-- Test 7; Extract name of specific player summary based on ID - non-existent player
test7 = TestCase $ assertEqual "Extract specific name of non-existent player" (Nothing) (nameOfPlayer 76561197888871024 testKeyValData)
-- Test 8; Extract names from summaries in same order as supplied ID:s
test8 = TestCase $ assertBool "Extract names in same order as supplied ID:s" ((orderedNames testParsedData [76561197979971024,76561197999847293]) == [Just "Maustronaut", Just "Avari"] && (orderedNames testParsedData [76561197999847293,76561197979971024]) == [Just "Avari",Just "Maustronaut"])
-- Test 9; Extract names from summaries using single ID
test9 = TestCase $ assertEqual "Extract names (in order) using single ID" [Just "Avari"] (orderedNames testParsedData [76561197999847293])
-- Test 10; Extract names from summaries using non-existing players
test10 = TestCase $ assertEqual "Extract names (in order) using non-existing players" [Nothing, Nothing] (orderedNames testParsedData [76561197847293, 99999999999])
-- Test 11; Extract names from summaries using no ID:s
test11 = TestCase $ assertEqual "Extract names (in order) using no ID:s" [] (orderedNames testParsedData [])
-- Test 12; Extract names from summaries using same ID multiple times
test12 = TestCase $ assertEqual "Extract names (in order) using same ID miltiple times" [Just "Avari",Just "Avari",Just "Avari",Just "Avari"] (orderedNames testParsedData [76561197999847293,76561197999847293,76561197999847293,76561197999847293])


steamSummariesTests = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12]