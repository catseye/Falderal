module Test.Falderal.Partitioner (partitionTests, isHaskellTest, isShellTest) where

import Test.Falderal.Common

--
-- Test partitioning functionality.
--

--
-- Given a list of tests and a list of predicates, create
-- separate lists of tests, one for each predicate.  Also
-- uniquely identify each test.
--

partitionTests preds tests =
    partitionTests' preds tests 1

partitionTests' [] tests id =
    []
partitionTests' (pred:preds) tests id =
    let
        (subTests, id') = filterTests pred tests id
    in
        (subTests:(partitionTests' preds tests id'))

filterTests pred [] id =
    ([], id)
filterTests pred (test@(Test _ fns _ _ _ _):rest) id =
    case filter (pred) fns of
        [] ->
           filterTests (pred) rest id
        fns' ->
            let
                (tests, id') = mapTests test fns' id
                (remainder, id'') = filterTests (pred) rest (id+1)
            in
                (tests ++ remainder, id'')
filterTests pred (_:rest) id =
    filterTests (pred) rest id

mapTests test [] id =
    ([], id)
mapTests test@(Test _ _ desc inp exp result) (fn:fns) id =
    let
        (remainder, id') = mapTests test fns (id+1)
    in
        ((Test id [fn] desc inp exp result):remainder, id')

--
-- Useful predicates to use, above.
--

isHaskellTest (HaskellTest _ _) = True
isHaskellTest _ = False

isShellTest (ShellTest _) = True
isShellTest _ = False
