#!/bin/sh

if [ "$1" == "-f"]; then
  cat <"$2"
elif [ "$1" == "-o"]; then
  cat >"$2"
else
  cat
fi
