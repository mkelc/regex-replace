module Test.ReplaceTest (tests) where

import Test.HUnit

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import qualified Text.Regex.TDFA as T
import qualified Text.Regex.Posix as P
import Text.Regex.Base
import Text.Regex.Replace
import Data.String

tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [("ReplaceAll   Test TDFA -1",  replaceAllTest testString1 (mkReTDFA testPat1) testRepl1 testRes1),
             ("ReplaceAll   Test POSIX-1", replaceAllTest testString1 (mkRePOSIX testPat1) testRepl1 testRes1),
             ("ReplaceAll   Test TDFA -2",  replaceAllTest testString1 (mkReTDFA testPat2) testRepl2 testRes2),
             ("ReplaceAll   Test POSIX-2", replaceAllTest testString1 (mkRePOSIX testPat2) testRepl2 testRes2),
             ("ReplaceAll   Test TDFA -3",  replaceAllTest testString1 (mkReTDFA testPat3) testRepl3 testRes3),
             ("ReplaceAll   Test POSIX-3", replaceAllTest testString1 (mkRePOSIX testPat3) testRepl3 testRes3),
             ("ReplaceFirst Test TDFA -1",  replaceFirstTest testString1 (mkReTDFA testPat1) testRepl1 testRes1a),
             ("ReplaceFirst Test POSIX-1", replaceFirstTest testString1 (mkRePOSIX testPat1) testRepl1 testRes1a),
             ("Excise Test TDFA -1",       exciseTest testString1 (mkReTDFA testPat4) testRes4),
             ("Excise Test POSIX-1",       exciseTest testString1 (mkRePOSIX testPat4) testRes4),
             ("Replace Test TDFA -1",      replaceTest testString1 (mkReTDFA testPat5) testRepl5 testRes5),
             ("Replace Test POSIX-1",      replaceTest testString1 (mkRePOSIX testPat5) testRepl5 testRes5)
             ]


replaceAllTest :: (Show a, Eq a, Monoid a, IsString a, RegexLike r a) => a-> r -> String -> a -> Test
replaceAllTest a r s a' = TestCase $ assertEqual "replaceAllTest" a' (replaceAll a r s)

replaceFirstTest :: (Show a, Eq a, Monoid a, IsString a, RegexLike r a) => a-> r -> String -> a -> Test
replaceFirstTest a r s a' = TestCase $ assertEqual "replaceFirstTest" a' (replaceFirst a r s)

exciseTest :: (Show a, Eq a, Monoid a, IsString a, RegexLike r a) => a-> r -> a -> Test
exciseTest a r a' = TestCase $ assertEqual "exciseTest" a' (excise a r)

replaceTest :: (Show a, Eq a, Monoid a, IsString a, RegexLike r a) => a-> r -> String -> a -> Test
replaceTest a r s a' = TestCase $ assertEqual "replaceTest" a' (replace a r s)

testString1 :: String
testString1 = ">FOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOB42BFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFO<"

testPat1 :: String
testPat1 = "(FO)+"

testRepl1 :: String
testRepl1 = ""

testRes1 :: String
testRes1 = ">B42B<"

testRes1a :: String
testRes1a = ">B42BFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFO<" 

testPat2 :: String
testPat2 = "B([[:digit:]]+)B"

testRepl2 :: String
testRepl2 = "-NUMBER $1-"

testRes2 :: String
testRes2 = ">FOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFO-NUMBER 42-FOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFO<"

testPat3 :: String
testPat3 = "(FO)+B([[:digit:]]+)B(FO)+"

testRepl3 :: String
testRepl3 = "NUMBER $2"

testRes3 :: String
testRes3 = ">NUMBER 42<"

testPat4 :: String
testPat4 = "B42B"

testRes4 :: String
testRes4 = ">FOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFOFO<"

testPat5 :: String
testPat5 = "FO"

testRepl5 :: String
testRepl5 = "A"

testRes5 :: String
testRes5 = ">AAAAAAAAAAAAAAAAAB42BAAAAAAAAAAAAAAAAAAA<"

mkReTDFA :: String -> T.Regex
mkReTDFA = T.makeRegex

mkRePOSIX :: String -> P.Regex
mkRePOSIX = P.makeRegex


