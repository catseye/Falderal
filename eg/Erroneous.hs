module Erroneous where

-- This Haskell module contains some errors which will cause the results
-- generator generated for it to not even compile.  Test.Falderal should
-- handle this in some way where it's obvious something went wrong.

countLines str = show $ length $ likes str
