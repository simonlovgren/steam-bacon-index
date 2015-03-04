{-  
    PKD 2014/2015 Project
    Group 30
    Simon Lövgren
    Erik Melander
    Fredrik Svensson

    NOTES:
        * Uses the underlying SteamAPI modules
-}
module Steam where
import KeyVal

import qualified SteamAPI.Requests (steamRequestsTests)  --  imported for unit testing purposes only
import qualified SteamAPI.Friends
import qualified SteamAPI.Summaries

import Test.HUnit

{-
    REPRESENTATION CONVENTION:
        * Represents a "64-bit" integer steam ID
    REPRESENTATION INVARIANT:
        * TRUE
-}
type SteamID = Integer


{-
	friends steamid

	PURPOSE:
		Return list of friends

	PRE:
		True

	POST:
		Returns IO list of KeyVal-lists containing steam friends data

    SIDEEFFECTS:
        * Relies on external data
        * Returns empty list if privacy of steam account is not set to public

	Examples:
		friends 76561198028357851 = IO [[KVInt "steamid" 76561197979971024,KVStr "relationship" "friend",KVInt "friend_since" 1424960766],[KVInt "steamid" 76561197989194839,KVStr "relationship" "friend",KVInt "friend_since" 1303555871],[KVInt "steamid" 76561198000124224,KVStr "relationship" "friend",KVInt "friend_since" 1336242822],[KVInt "steamid" 76561198043343260,KVStr "relationship" "friend",KVInt "friend_since" 1419003931]]

		friends 123 = []
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

    SIDEEFFECTS:
        * Relies on external data
        * Returns empty list if privacy of steam account is not set to public

	Examples:
		friendsIDs 76561198028357851 = IO [76561197979971024,76561197989194839,76561198000124224,76561198043343260]
-}
friendsIDs :: SteamID -> IO [SteamID]
friendsIDs id = do
	friends <- SteamAPI.Friends.getIDList id
	return $ friends


{-
	playersExist list

	PURPOSE:
		Return player summaries for supplied ID:s 

	PRE:
		True

	POST:
		Returns an IO Bool. True if all ID:s resolve to a steam user, otherwise false.

    SIDEEFFECTS:
        * Relies on external data
        * Returns empty list if privacy of steam account is not set to public

	Examples:
		playersExist [76561198028357851,76561197979971024] = IO True
		playersExist [76561198028357851,123456789] = IO False

		playersExist [] == IO False and prints an error message
-}
playersExist :: [SteamID] -> IO Bool
playersExist list = SteamAPI.Summaries.playersExist list

{-
	playerSummaries list

	PURPOSE:
		Return player summaries for supplied ID:s 

	PRE:
		* A minimum of 1 SteamID in list
		* A maximum of 100 SteamID:s in list

	POST:
		Returns IO list of KeyVal-lists containing steam player summaries

    SIDEEFFECTS:
        * Relies on external data
		* May return summaries in order different from ID:s in list
        * Steam accounts with privacy other than public will not be returned in list

	Examples:
		playerSummaries [76561197979971024] = IO [[KVInt "steamid" 76561197979971024,KVStr "personaname" "Maustronaut",KVInt "lastlogoff" 1425457171,KVStr "profileurl" "http://steamcommunity.com/profiles/76561197979971024/",KVStr "avatar" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg",KVStr "avatarmedium" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg",KVStr "avatarfull" "https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg"]]

		playerSummaries [] = [] and prints an error message
-}
playerSummaries :: [SteamID] -> IO [[KeyVal]]
playerSummaries ids = SteamAPI.Summaries.getPlayerList ids

{-
	playerNamesOrdered list

	PURPOSE:
		Return player personanames for suppied ID:s in same order as ID:s 

	PRE:
		* A minimum of 1 SteamID in list
		* A maximum of 100 SteamID:s in list

	POST:
		* Returns IO list of Maybe String. Just "personaname" if personaname was found, otherwise Nothing

    SIDEEFFECTS:
        * Relies on external data
        * Steam accounts with privacy other than public will not be returned in list

	Examples:
		playerNamesOrdered [76561198028357851,76561197979971024,76561197999847293] = IO [Just "Erikun", Just "Maustronaut", Just "Avari"]
		playerNamesOrdered [76561197999847293, 76561197979971024, 76561198028357851] = IO [Just "Avari", Just "Maustronaut", Just "Erikun"]

		playerNamesOrdered [76561197999847293, 55666, 76561198028357851] = IO [Just "Avari", Nothing, Just "Erikun"]
		playerNamesOrdered [] = IO [] and prints an error message
-}
playerNamesOrdered :: [SteamID] -> IO [Maybe String]
playerNamesOrdered ids = SteamAPI.Summaries.getOrderedNames ids




{-
	TODO : Prepare test cases?
-}