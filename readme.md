# File Trigger 

Execute shell script on file system events in some directory

Toy project developed during university course "Corporate Software Development"

Subgoal of this project is to explore [Extensible Effects](https://hackage.haskell.org/package/extensible-effects) library.

## Example of config: 
    
    directory: .
    logFileName: ./log/app.log
    refreshRate: 1000000
    onCreateScript: ./onCreate.sh
    onRemoveScript: ./onRemove.sh
    onModifyScript: ./onModify.sh
    ignore: [".","..","app.log"]

## How to build: 

1. Install Haskell Platform 
2. `git clone` this repository or download zip, enter containing directory
3. `cabal sandbox init`
4. `cabal install`  