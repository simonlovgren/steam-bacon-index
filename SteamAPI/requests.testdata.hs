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
	(getPlayerSummaries,
	getFriendList,
	SteamID,
	AppID,
	steamReqTests)
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

{-
	concatIDs ids

	PURPOSE:
		Concatenate ids and return as comma separated strings.

	PRE:
		* TRUE

	POST:
		* TRUE

	EXAMPLES:
		concatIDs [123456789, 2345678, 34567890] == "123456789,2345678,34567890"
-}
concatIDs :: [Integer] -> String
concatIDs [] = []
concatIDs (x:[]) = (show x) ++ []
concatIDs (x:xs) = (show x) ++ ',' : concatIDs xs

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
getPlayerSummaries ids = do { return $ "{\n\t\"response\": {\n\t\t\"players\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197979971024\",\n\t\t\t\t\"communityvisibilitystate\": 3,\n\t\t\t\t\"profilestate\": 1,\n\t\t\t\t\"personaname\": \"Maustronaut\",\n\t\t\t\t\"lastlogoff\": 1425457171,\n\t\t\t\t\"profileurl\": \"http://steamcommunity.com/profiles/76561197979971024/\",\n\t\t\t\t\"avatar\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8.jpg\",\n\t\t\t\t\"avatarmedium\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_medium.jpg\",\n\t\t\t\t\"avatarfull\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/fb/fb987dbeea879f2bfb733fc2ae218c2c9d9bc0c8_full.jpg\",\n\t\t\t\t\"personastate\": 0,\n\t\t\t\t\"realname\": \"Simon Lövgren\",\n\t\t\t\t\"primaryclanid\": \"103582791431467770\",\n\t\t\t\t\"timecreated\": 1135541046,\n\t\t\t\t\"personastateflags\": 0,\n\t\t\t\t\"loccountrycode\": \"SE\",\n\t\t\t\t\"locstatecode\": \"21\",\n\t\t\t\t\"loccityid\": 43767\n\t\t\t},\n\t\t\t\t{\n\t\t\t\t\"steamid\": \"76561197999847293\",\n\t\t\t\t\"communityvisibilitystate\": 3,\n\t\t\t\t\"profilestate\": 1,\n\t\t\t\t\"personaname\": \"Avari\",\n\t\t\t\t\"lastlogoff\": 1425417881,\n\t\t\t\t\"profileurl\": \"http://steamcommunity.com/profiles/76561197999847293/\",\n\t\t\t\t\"avatar\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457.jpg\",\n\t\t\t\t\"avatarmedium\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_medium.jpg\",\n\t\t\t\t\"avatarfull\": \"https://steamcdn-a.akamaihd.net/steamcommunity/public/images/avatars/9e/9e3cc9976bbab0365d6d093ee9c9d8c1db041457_full.jpg\",\n\t\t\t\t\"personastate\": 1,\n\t\t\t\t\"primaryclanid\": \"103582791430380703\",\n\t\t\t\t\"timecreated\": 1215871590,\n\t\t\t\t\"personastateflags\": 0,\n\t\t\t\t\"loccountrycode\": \"SE\",\n\t\t\t\t\"locstatecode\": \"21\",\n\t\t\t\t\"loccityid\": 43767\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"}

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
getFriendList id = do { return $ "{\n\t\"friendslist\": {\n\t\t\"friends\": [\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197963805864\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197963864986\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197964446578\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288114261\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197965528292\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1223307109\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197966096475\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1377453954\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197970605876\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197970955861\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197972250120\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288365378\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197979954514\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1420375885\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197980774938\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1361041135\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197982771513\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197985084188\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288520265\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197993382966\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1421956433\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197993438981\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 0\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197994186451\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288121374\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561197999847293\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1421930727\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198003329129\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288021544\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198013927921\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1281713136\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198018381280\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1287934076\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198028357851\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1424960766\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198028886797\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1281713240\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198032449877\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1288204985\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198044924691\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1324914853\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198055561229\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1361041646\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198066601598\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1343250156\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198067037291\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1353535204\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198120393817\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1388781095\n\t\t\t},\n\t\t\t{\n\t\t\t\t\"steamid\": \"76561198159593485\",\n\t\t\t\t\"relationship\": \"friend\",\n\t\t\t\t\"friend_since\": 1413966223\n\t\t\t}\n\t\t]\n\t\t\n\t}\n}"}



-- Test 1; Test concatenation of ID:s used for calls with multiple ID:s in same call
test1 = TestCase $ assertEqual "Concatenate integer list" "123456789,2345678,34567890" (concatIDs [123456789, 2345678, 34567890])

steamReqTests = TestList $ [test1]