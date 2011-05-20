#!/bin/sh

# A tiny test harness for Falderal itself.

ghc Test/Falderal/Demo.lhs -e test >actual.txt
diff -u expected.txt actual.txt
