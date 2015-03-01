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
#! haskell
import qualified Steam

-- get list of friends IDs:
Steam.friendsIDs 76561197979971024  -- fetches friends as [Integer]

Steam.friends 76561197979971024 	-- fetch friends as [[KeyVal]]

Steam.resolveVanity "erikun"		-- return steam ID as Integer

-- ... etc ... --
```


## Git tutorials:
[https://www.atlassian.com/git/tutorials/](https://www.atlassian.com/git/tutorials/)