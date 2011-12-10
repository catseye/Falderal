#!/bin/sh

if [ "x$1" = "xclean" ]; then
    cabal clean
fi

cabal install --prefix=$HOME --user && cabal clean
