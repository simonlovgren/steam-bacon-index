{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson


    NOTES:
        * Calls to the WebAPI is made using Network.HTTP
        * General description of the package was used to understand implementation
        * Key can be found/created at: http://steamcommunity.com/dev/apikey

		* KEY (Simon/slovgren.com)	:: F54C9824025C1E667C45B6887D7765B0
		* SteamID (Simon/Maustronaut)			:: 76561197979971024
-}

module SteamAPI.Requests 
	(getSteamIdFromVanityURL,
	getPlayerSummaries,
	getFriendList,
	getOwnedGames,
	getPlayerAchievements,
	getUserStatsForGame,
	getRecentlyPlayedGames,
	getPlayerBans,
	getSteamApps,
	getAppSchema,
	SteamID,
	AppID,
	steamRequestsTests)
	where

import Network.HTTP
import Data.List

import Test.HUnit

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer
{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam application ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type AppID = Integer



-- BASE SETTINGS --
{-
	key

	PURPOSE:
		Return API key for ease to change key 

	PRE:
		True

	POST:
		Return the API key as a string
-}
key 	= "F54C9824025C1E667C45B6887D7765B0" -- Steam API Key (Simon/slovgren.com), can be found/created @ http://steamcommunity.com/dev/apikey
{-
	apiBase

	PURPOSE:
		Return API base URL for ease to change if a new URL is required

	PRE:
		True

	POST:
		Return the API base URL as a string
-}
apiBase = "http://api.steampowered.com"

-- Core functionality --
{-
	get url

	PURPOSE:
		Fetch request body by URL (url).

	PRE:
		A correctly formatted URL is supplied

	SIDE EFFECTS:
		Fetches data from Web API using SimpleHTML request

	POST:
		Returns request body in form of a IO String

-}
get :: String -> IO String
get url = do
	resp <- simpleHTTP (getRequest url)
	getResponseBody resp

{-
	concatIDs ids

	PURPOSE:
		Concatenate ids as comma separated string.

	PRE:
		TRUE

	POST:
		Returns ID:s formatted as comma separated string.

	EXAMPLES:
		concatIDs [123456789, 2345678, 34567890] == "123456789,2345678,34567890"
-}
concatIDs :: [Integer] -> String
concatIDs [] = []
concatIDs (x:[]) = (show x) ++ []
concatIDs (x:xs) = (show x) ++ ',' : concatIDs xs


{-
	getSteamIdFromVanityURL vanityname

	PURPOSE:
		Fetch player steamid using vanity name (custom name/url).

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data

-}
getSteamIdFromVanityURL :: String -> IO String
getSteamIdFromVanityURL vanity = get (apiBase ++ "/ISteamUser/ResolveVanityURL/v0001/?key=" ++ key ++ "&vanityurl=" ++ vanity)


{-
	getPlayerSummaries steamIDs

	PURPOSE:
		Fetch player profile summaries.

	PRE:
		steamIDs contain at least one ID
		steamIDs contain at most 100 IDs

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getPlayerSummaries :: [SteamID] -> IO String
getPlayerSummaries ids = get (apiBase ++ "/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ key ++ "&steamids=" ++ (concatIDs ids))

{-
	getFriendList steamID

	PURPOSE:
		Fetch list of friends/connections to steam user with steamID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getFriendList :: SteamID -> IO String
getFriendList id = get (apiBase ++ "/ISteamUser/GetFriendList/v0001/?key=" ++ key ++ "&relationship=friend&steamid=" ++ show(id))

{-
	getOwnedGames steamID

	PURPOSE:
		Fetch list of games owned by steam user with steamID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getOwnedGames :: SteamID -> IO String
getOwnedGames id = get (apiBase ++ "/IPlayerService/GetOwnedGames/v0001/?key=" ++ key ++ "&steamid=" ++ show(id))

{-
	getPlayerAchievements steamID appID

	PURPOSE:
		Fetch list of achievements and their status from app appID for steam user with steamID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getPlayerAchievements :: SteamID -> AppID -> IO String
getPlayerAchievements id appid = get (apiBase ++ "/ISteamUserStats/GetPlayerAchievements/v0001/?key=" ++ key ++ "&appid=" ++ show(appid) ++ "&steamid=" ++ show(id))

{-
	getUserStatsForGame steamID appID

	PURPOSE:
		Fetch user statistics for app with appID for steam user with steamID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getUserStatsForGame :: SteamID -> AppID -> IO String
getUserStatsForGame id appid = get (apiBase ++ "/ISteamUserStats/GetUserStatsForGame/v0002/?key=" ++ key ++ "&appid=" ++ show(appid) ++ "&steamid=" ++ show(id))

{-
	getRecentlyPlayedGames steamID

	PURPOSE:
		Fetch games played in the last two weeks for steam user with steamID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getRecentlyPlayedGames :: SteamID -> IO String
getRecentlyPlayedGames id = get (apiBase ++ "/IPlayerService/GetRecentlyPlayedGames/v0001/?key=" ++ key ++ "&steamid=" ++ show(id))

{-
	getPlayerBans steamIDs

	PURPOSE:
		Fetch ban status for steamIDs.

	PRE:
		steamIDs contain at least one ID
		steamIDs contain at most 100 IDs

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getPlayerBans :: [SteamID] -> IO String
getPlayerBans ids = get (apiBase ++ "/ISteamUser/GetPlayerBans/v1/?key=" ++ key ++ "&steamids=" ++ (concatIDs ids))

{-
	getSteamApps

	PURPOSE:
		Fetch list containing all apps available on steam.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getSteamApps :: IO String
getSteamApps = get (apiBase ++ "/ISteamApps/GetAppList/v2")

{-
	getAppSchema appID

	PURPOSE:
		Fetch info about game with app id appID.

	PRE:
		TRUE

	POST:
		Returns request body in form of a IO String containing JSON-formatted/serialized data-

-}
getAppSchema :: AppID -> IO String
getAppSchema appid = get (apiBase ++ "/ISteamUserStats/GetSchemaForGame/v2/?key=" ++ key ++ "&appid=" ++ show(appid))




-- Test 1; Test concatenation of ID:s used for calls with multiple ID:s in same call
test1 = TestCase $ assertEqual "Concatenate integer list" "123456789,2345678,34567890" (concatIDs [123456789, 2345678, 34567890])
-- Test 2; Test concatenation of ID:s used for calls with single ID
test2 = TestCase $ assertEqual "Concatenate single entry integer list" "123456789" (concatIDs [123456789])
-- Test 3; Test concatenation of ID:s used for calls with no ID:s
test3 = TestCase $ assertEqual "Concatenate empty (integer) list" "" (concatIDs [])

steamRequestsTests = TestList [test1,test2,test3]