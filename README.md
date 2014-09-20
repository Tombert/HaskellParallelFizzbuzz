HaskellParallelFizzbuzz
=======================

To play with the parallel abilities of Haskell, here's a basic fizzbuzz test implemented (mostly) parallel.  

Running
=======
This is pretty basic Haskell, so there's no real need to do anything very
Cabal-ey here.  Simply run `ghc main.hs -threaded --make main.hs`, then run
`./main +RTS -N4` (replace `-N4` with the appropriate `N` for the number of
cores that you have).  Bam. You're done. 
