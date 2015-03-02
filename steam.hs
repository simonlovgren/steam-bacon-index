{-  
    Group 30
    Simon Lövgren
    February 2015

    NOTES:
        * Uses the underlying SteamAPI modules

    TODO : Review document
-}
module Steam where
import KeyVal

import qualified SteamAPI.Friends
import qualified SteamAPI.Summaries

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer


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
friends :: SteamID -> IO [[KeyVal]]
friends id = do
	friends <- SteamAPI.Friends.getRawList id
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
playerSummaries :: [SteamID] -> IO [[KeyVal]]
playerSummaries ids = SteamAPI.Summaries.getPlayerList ids




-- TODO : Add test cases