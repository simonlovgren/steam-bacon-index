## Project dependencies
**Text.JSON**

Install using:
```
#!
cabal install json
```
Or for global installation:
```
#!
cabal install json --global
```

## Usage of modules
**Steam API**

Import- and use steam API from file *steam* in root folder:
```
#!haskell
import qualified Steam

-- get list of friends IDs:
Steam.friendsIDs 76561197979971024  -- fetches friends as IO [Integer]

-- get list of friends:
Steam.friends 76561197979971024 	-- fetch friends as IO [[KeyVal]] list

-- check if users exist on Steam:
Steam.playersExist [76561197979971024,76561197979971024,76561197979971024]	-- Check if users in supplied list exists IO Bool

-- retreive steam user summaries:
Steam.playerSummaries [76561197979971024]	-- Retreive player summaries as [[KeyVal]] list
```

** EXAMPLE: Retreiving personaname from [[KeyVal]] list**
Example of retreiving names from list of Steam ID:s using Steam.playerSummaries and KeyVal-module:
```
#!haskell

-- Retreive friends and unwrap IO Monad (to use as an example list)
friends <- Steam.friendsIDs 76561197979971024
-- Push friends to playerSummaries and unwrap IO Monad to expose [[KeyVal]] list
players <- Steam.playerSummaries friends

-- Map KeyVal.findKVString over [[KeyVal]] list with search argument "personaname"
map (flip findKVString "personaname") players
	-- = [Maybe String] containing Maybe (personaname) instead of KeyVal structure 

```

## Git tutorials:
[https://www.atlassian.com/git/tutorials/](https://www.atlassian.com/git/tutorials/)