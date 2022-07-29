#!/bin/sh

if [ "x$1" = "x-n" ]; then
  printf %s "$2"
else
  printf %s\\n "$1"
fi
