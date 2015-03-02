{-  
    Group 30
    Simon Lövgren
    February 2015

    NOTES:
        * Uses the underlying SteamAPI modules



    NOTE: Under construction
    TODO: Add exports when file is done?
-}
module Steam where

import qualified SteamAPI.Friends
import qualified SteamAPI.Summaries
{-
	
	TODO: Add import of all new interfaces here
	
	import qualified SteamAPI.ResolveVanity
	import qualified SteamAPI.Player

	etc. etc.

-}

{-
	Declare types for clarification
-}
type VanityTag = String
type SteamID = Integer
type AppID = Integer
-- unknown used for yet not-written or thought of functions
data Unknown = Unknown deriving (Show)

-- ################### HELPERS ################### --
{-
	resolveVanity vanitytag

	PURPOSE:
		Return Steam ID based on vanity URL tag 

	PRE:
		True

	POST:
		Not yet determined, but probably IO (Maybe Integer)
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
resolveVanity :: VanityTag -> IO (Maybe Integer)
resolveVanity _ = do
	putStrLn "Not yet created"
	return Nothing

-- ################### FRIENDS ################### --
{-
	getFriends steamid

	PURPOSE:
		Return list of friends

	PRE:
		True

	POST:
		Not yet determined, but probably list of list of tuples [[(name,value)]]
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
friends :: SteamID -> IO Unknown
friends id = do
	friends <- SteamAPI.Friends.getFriends id
	return friends

{-
	friendsIDs steamid

	PURPOSE:
		Return list of friends Steam ID:s

	PRE:
		True

	POST:
		Returns list of SteamID:s
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
friendsIDs :: SteamID -> IO [SteamID]
friendsIDs id = do
	friends <- SteamAPI.Friends.getIDList id
	return $ friends


-- ################### PLAYER ################### --
{-
	playersExist list

	PURPOSE:
		Return player summaries for suppied ID:s 

	PRE:
		True

	POST:
		Not yet determined, but probably IO (Maybe Integer)
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
playersExist :: [SteamID] -> IO Bool
playersExist list = SteamAPI.Summaries.playersExist list

{-
	playerSummaries list

	PURPOSE:
		Return player summaries for suppied ID:s 

	PRE:
		True

	POST:
		Not yet determined, but probably IO (Maybe Integer)
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
playerSummaries :: [SteamID] -> IO Unknown
playerSummaries _ = do
	putStrLn "Not yet created"
	return Unknown

{-
	ownedGames steamid

	PURPOSE:
		Return list of owned games for steamid

	PRE:
		True

	POST:
		Not yet determined, but probably list of list of tuples [[(name,value)]]
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
ownedGames :: SteamID -> IO Unknown
ownedGames _ = do
	putStrLn "Not yet created"
	return Unknown

{-
	ownedGames steamid

	PURPOSE:
		Return list of IDs of owned games for steamid

	PRE:
		True

	POST:
		Returns list of AppID
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
ownedGamesIDs :: SteamID -> IO [AppID]
ownedGamesIDs _ = do
	putStrLn "Not yet created"
	return []

{-
	playerAchievements steamid

	PURPOSE:
		Return list of steam achievements for steamid 

	PRE:
		True

	POST:
		Not yet determined, but probably list of list of tuples [[(name,value)]]
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
playerAchievements :: SteamID -> IO Unknown
playerAchievements _ = do
	putStrLn "Not yet created"
	return Unknown

{-
	recentlyPlayed steamid

	PURPOSE:
		Return list of games played the last two weeks for steamid

	PRE:
		True

	POST:
		Not yet determined, but probably list of list of tuples [[(name,value)]]
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
recentlyPlayed :: SteamID -> IO Unknown
recentlyPlayed _ = do
	putStrLn "Not yet created"
	return Unknown

{-
	recentlyPlayedIDs steamid

	PURPOSE:
		Return list of games played the last two weeks for steamid

	PRE:
		True

	POST:
		Returns list of AppID
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
recentlyPlayedIDs :: SteamID -> IO [AppID]
recentlyPlayedIDs _ = do
	putStrLn "Not yet created"
	return []

-- ################### GAMES ################### --
{-
	appSchema appid

	PURPOSE:
		Return details about steam app

	PRE:
		True

	POST:
		Not yet determined, but probably list of list of tuples [[(name,value)]]
	
	SIDE EFFECTS:
		--

	Examples:
		--
-}
appSchema :: AppID -> IO Unknown
appSchema _ = do
	putStrLn "Not yet created"
	return Unknown

-- ################### OTHER ################### --
{-
	NOTE: There are some more functions that could be included, but might not be nessesary at this stage:
			* steamApps = List of all (~15500) apps on steam
			* playerBans = If player has been banned
			* getUserStatsForGame = statistics about specifig game for specified user
-}