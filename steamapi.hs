module SteamAPI where

import Network.HTTP
import Data.List

-- Key can be found/created at: http://steamcommunity.com/dev/apikey

-- KEY (Simon/slovgren.com)	:: F54C9824025C1E667C45B6887D7765B0
-- SteamID (Simon)			:: 76561197979971024

-- Data types --
type SteamID = Integer
type AppID = Integer

-- BASE SETTINGS --
key 	= "F54C9824025C1E667C45B6887D7765B0" -- Steam API Key (Simon/slovgren.com), can be found/created @ http://steamcommunity.com/dev/apikey
apiBase = "http://api.steampowered.com"

-- Core functionality --
{-
	get url

	PURPOSE:
		Fetch request body by URL (url).

	PRE:
		* A correctly formatted URL is supplied

	SIDE EFFECTS:
		* Fetches data from Web API using SimpleHTML request

	POST:
		* Returns request body in form of a IO String

	EXAMPLES:
		---

-}
get :: String -> IO String
get url = do
	resp <- simpleHTTP (getRequest url)
	getResponseBody resp

{-
	code url

	PURPOSE:
		Fetch response code from URL (url).

	PRE:
		* A correctly formatted URL is supplied

	SIDE EFFECTS:
		* Fetches data from Web API using SimpleHTML request

	POST:
		* Returns response code from given URL (url) and returns it as a triple value tuple.

	EXAMPLES:		
		code "http://google.com" 		== (3,0,2) 		-- HTTP Code 302 Found (Redirection)
		code "http://google.com/lorem" 	== (4,0,4) 		-- HTTP Code 404 Not Found
		code "http://google.se" 		== (3,0,1) 		-- HTTP Code 301 Moved Permanently
		code "http://www.google.se" 	== (2,0,0) 		-- HTTP Code 200 OK
		
		code "http://gog98897ole.se" 	== Throws exeption (host lookup failure)
-}
code :: String -> IO ResponseCode
code url = do
	resp <- simpleHTTP (getRequest url)
	getResponseCode resp


{-
	printRequestBody string

	PURPOSE:
		Print the request body to console/terminal with correct formatting.

	PRE:
		* TRUE

	POST:
		* TRUE

	EXAMPLES:
		---
-}
printRequestBody :: IO String -> IO ()
printRequestBody iostr = do
	str <- iostr
	putStr (str)


{-
	concatIDs ids

	PURPOSE:
		Concatinate ids and return as comma separated strings.

	PRE:
		* TRUE

	POST:
		* TRUE

	EXAMPLES:
		concatIDs [123456789, 2345678, 34567890] == "123456789,2345678,34567890"
-}
concatIDs :: [Integer] -> String
concatIDs [] = []
concatIDs (x:xs) = (show x) ++ ',' : concatIDs xs


-- Specific API calls --
-- IPlayerService -- PLAYERINFO -- 
-- http://api.steampowered.com/ISteamUser/ResolveVanityURL/v0001/?key=F54C9824025C1E667C45B6887D7765B0&vanityurl=erikun
{-
	getSteamIdFromVanityURL vanityname

	PURPOSE:
		Fetch player steamid using vanity name (custom name/url).

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getSteamIdFromVanityURL :: String -> IO String
getSteamIdFromVanityURL vanity = get (apiBase ++ "/ISteamUser/ResolveVanityURL/v0001/?key=" ++ key ++ "&vanityurl=" ++ vanity)


{-
	getPlayerSummaries steamIDs

	PURPOSE:
		Fetch player profile summaries.

	PRE:
		* steamIDs contain at least one ID
		* steamIDs contain at most 100 IDs

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getPlayerSummaries :: [SteamID] -> IO String
getPlayerSummaries ids = get (apiBase ++ "/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ key ++ "&steamids=" ++ (concatIDs ids))

{-
	getFriendList steamID

	PURPOSE:
		Fetch list of friends/connections to steam user with steamID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getFriendList :: SteamID -> IO String
getFriendList id = get (apiBase ++ "/ISteamUser/GetFriendList/v0001/?key=" ++ key ++ "&relationship=friend&steamid=" ++ show(id))

{-
	getOwnedGames steamID

	PURPOSE:
		Fetch list of games owned by steam user with steamID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getOwnedGames :: SteamID -> IO String
getOwnedGames id = get (apiBase ++ "/IPlayerService/GetOwnedGames/v0001/?key=" ++ key ++ "&steamid=" ++ show(id))

{-
	getPlayerAchievements steamID appID

	PURPOSE:
		Fetch list of achievements and their status from app appID for steam user with steamID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getPlayerAchievements :: SteamID -> AppID -> IO String
getPlayerAchievements id appid = get (apiBase ++ "/ISteamUserStats/GetPlayerAchievements/v0001/?key=" ++ key ++ "&appid=" ++ show(appid) ++ "&steamid=" ++ show(id))

{-
	getUserStatsForGame steamID appID

	PURPOSE:
		Fetch user statistics for app appID for steam user with steamID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getUserStatsForGame :: SteamID -> AppID -> IO String
getUserStatsForGame id appid = get (apiBase ++ "/ISteamUserStats/GetUserStatsForGame/v0002/?key=" ++ key ++ "&appid=" ++ show(appid) ++ "&steamid=" ++ show(id))

{-
	getRecentlyPlayedGames steamID

	PURPOSE:
		Fetch games played in the last two weeks for steam user with steamID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getRecentlyPlayedGames :: SteamID -> IO String
getRecentlyPlayedGames id = get (apiBase ++ "/IPlayerService/GetRecentlyPlayedGames/v0001/?key=" ++ key ++ "&steamid=" ++ show(id))

{-
	getPlayerBans steamIDs

	PURPOSE:
		Fetch ban status for steamIDs.

	PRE:
		* steamIDs contain at least one ID
		* steamIDs contain at most 100 IDs

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getPlayerBans :: [SteamID] -> IO String
getPlayerBans ids = get (apiBase ++ "/ISteamUser/GetPlayerBans/v1/?key=" ++ key ++ "&steamids=" ++ (concatIDs ids))


-- ISteamApps -- App Info--
{-
	getSteamApps

	PURPOSE:
		Fetch list containing all apps available on steam.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getSteamApps :: IO String
getSteamApps = get (apiBase ++ "/ISteamApps/GetAppList/v2")

{-
	getAppSchema appID

	PURPOSE:
		Fetch info about game with app id appID.

	PRE:
		* TRUE

	POST:
		* Returns request body in form of a IO String containing JSON-formatted/serialized data

	EXAMPLES:
		---

-}
getAppSchema :: AppID -> IO String
getAppSchema appid = get (apiBase ++ "/ISteamUserStats/GetSchemaForGame/v2/?key=" ++ key ++ "&appid=" ++ show(appid))