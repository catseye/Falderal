#!/bin/sh

if [ "x$1" = "x-f" ]; then
  if [ "x$3" = "x-o" ]; then
    cat <"$2" >"$4"
  else
    cat <"$2"
  fi
elif [ "x$1" = "x-o" ]; then
  cat >"$2"
else
  cat
fi
