#!/bin/sh

cabal clean && cabal install --prefix=$HOME --user && cabal clean
