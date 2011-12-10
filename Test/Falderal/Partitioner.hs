module Test.Falderal.Partitioner (partitionTests, isHaskellFunctionality, isShellFunctionality) where

import Test.Falderal.Common

--
-- Test partitioning functionality.
--

--
-- Given a list of tests and a list of predicates on their functionality,
-- create separate lists of tests, one for each functionality.  Also
-- uniquely identify each test by assigning it a unique integer ID.
--

partitionTests :: [Block -> Bool] -> [Block] -> [[Block]]
partitionTests preds tests =
    let
        tests' = singulate tests
        testLists = partition preds tests'
        numberedTests = numberTests testLists 1
    in
        numberedTests

--
-- Given a list of tests, each of which might have multiple functionalities
-- implementing it, return a (possibly longer) list of tests, each of which
-- is implemented by a single functionality.
--

singulate :: [Block] -> [Block]
singulate [] =
    []
singulate ((Test id fns desc inp exp result):tests) =
    let
        newTests = map (\fn -> Test id [fn] desc inp exp result) fns
    in
        newTests ++ (singulate tests)
singulate (_:tests) =
    singulate tests

partition :: [Block -> Bool] -> [Block] -> [[Block]]
partition [] tests =
    []
partition (pred:preds) tests =
    (filter (pred) tests:partition preds tests)

numberTests [] id =
    []
numberTests (list:lists) id =
    let
        (list', id') = numberTestList list id
    in
        (list':numberTests lists id')

numberTestList [] id =
    ([], id)
numberTestList ((Test _ fns desc inp exp result):tests) id =
    let
        (remainder, id') = numberTestList tests (id+1)
    in
        ((Test id fns desc inp exp result):remainder, id')

--
-- Useful predicates to use, above.
--

isHaskellFunctionality (Test _ [(HaskellTest _ _)] _ _ _ _) = True
isHaskellFunctionality _ = False

isShellFunctionality (Test _ [(ShellTest _)] _ _ _ _) = True
isShellFunctionality _ = False
