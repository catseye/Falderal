#!/bin/sh

# This Bourne shell script should fail when run, unless you have some really
# weirdly-named executables on your $PATH.  Test.Falderal should interpret
# this failure as an exception.

echo 1>&2 gello
exit 1
