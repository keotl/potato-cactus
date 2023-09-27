# Debugging
## Running with stacktraces
```
cabal configure --enable-profiling --profiling-detail toplevel-functions
cabal run potato-cactus -- +RTS -xc
```

## With memory profiling
1. Add -rtsopts in ghc-options in .cabal file.
2.
```
cabal run potato-cactus -- +RTS -hc -i1 -L1000
# create/view the report
hp2ps -c -M potato-cactus.hp
ps2pdf potato-cactus.ps
xdg-open potato-cactus.pdf
```

https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/prof-heap.html

## With CPU profiling
```
cabal run potato-cactus -- +RTS -pj
```

load the .prof file into https://www.speedscope.app/
