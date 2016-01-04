#! /bin/bash

#Bad idea:
#valgrind --tool=callgrind ./.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/Rosalyn-exe/Rosalyn-exe -N 1

stack clean
stack build --enable-library-profiling --enable-executable-profiling --ghc-options="-rtsopts" --ghc-options="-fprof-auto"
./.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/Rosalyn-exe/Rosalyn-exe +RTS -Pa

