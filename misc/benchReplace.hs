{-#LANGUAGE FlexibleContexts #-}
-- |Generate a big random string and perform matches and replacements on it
module Main where
import Control.Monad.State
import System.Random.Mersenne.Pure64
import Text.Regex.TDFA
import Text.Regex.Replace
import Criterion.Main
import Data.String
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as C

-- | Generate an infinite random string using a PureMT state and lazy recursion
cs :: [Char]
cs = ['0'..'z']

ls :: Int
ls = length cs 

randString :: PureMT -> String
randString p = let rec f p =  let (r,p') = randomInt p
                                  i      = (abs r) `mod` ls
                              in (head (drop i cs)) : (f p')
               in fix rec $ p

randBString :: PureMT -> C.ByteString
randBString p = let (r,p')  = randomInt p
                    i       = (abs r) `mod` ls
                in (head (drop i cs)) `C.cons` (randBString p')

pat :: Regex
pat = makeRegex "[^a]+"

-- | make a simple replacement: sequences of strings not containing an a are replaced by @
--   The result should be a string like aaa@a@aa@aaa@a@a@@@a.
--   This procedure was selected to have pretty much append operations on one big string
--   Note the declaration of transf which itsself is not aware of Strings or ByteString
transf :: (Eq s, IsString s, Monoid s, RegexLike Regex s) => s -> s
transf s = replaceAll s pat "@"

main :: IO ()
main = do 
   p <- newPureMT
   let str= randString p
   defaultMain $ [
      bgroup "Strings" (map (\n-> bench ("match random string" ++ (show n)) $ nfIO (calcRandom n)) [100000,200000..1000000]),
      bgroup "ByteStrings" (map (\n-> bench ("match random bytestring " ++ (show n)) $ nfIO (calcRandomB n)) [100000,200000..1000000])]

calcRandom :: Int -> IO ()
calcRandom n = do
   p <- newPureMT
   let str= randString p
       l  = length $ transf (take n str)
   putStrLn $ "Len = " ++ (show l)
   return ()

calcRandomB :: Int64 -> IO ()
calcRandomB n = do
   p <- newPureMT
   let str= randBString p
       l  = C.length $ transf (C.take n str)
   putStrLn $ "Len = " ++ (show l)
   return ()
