#!/bin/sh

if [ "x$1" = "xclean" ]; then
    cabal clean
fi

cabal install --prefix=C:\\cygwin\\home\\$USER\\ --user && cabal clean
mv $HOME/bin/falderal.exe $HOME/bin/falderal

