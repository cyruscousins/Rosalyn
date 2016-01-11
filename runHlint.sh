#! /bin/bash

#hlint src/*.hs src/*/*.hs --hint=Default --hint=Dollar --hint=Generalise > hlint.out
#hlint src/*.hs src/*/*.hs --hint=Default --ignore="Use Infix" --ignore="Use concatMap" > hlint.out
hlint src/*.hs src/*/*.hs src/*/*/*.hs --hint=Default --ignore="Use Infix" --ignore="Use concatMap" > hlint.out
