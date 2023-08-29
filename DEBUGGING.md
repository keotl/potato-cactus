# Debugging
## Running with stacktraces
```
cabal configure --enable-profiling --profiling-detail toplevel-functions
cabal run potato-cactus -- +RTS -xc
```
