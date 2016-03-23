{-|
Uses monad fix to generate an infinte random lazy string 
and generates a big random file
-}
module Main where
import Control.Monad.State
import System.Random.Mersenne.Pure64
import Text.Regex.TDFA
import Text.Regex.Replace
import Criterion.Main

-- | Generate an infinite random string using a PureMT state and MonadFix
randString :: PureMT -> String
randString p = let cs      = ['0'..'z']
                   ls      = length cs
                   rec f p =  let (r,p') = randomInt p
                                  i      = (abs r) `mod` ls
                              in (head (drop i cs)) : (f p')
               in fix rec $ p

pat :: Regex
pat = makeRegex "m([[:digit:]]+)"

transf :: String -> String
transf s = replaceAll s pat "FOUND:(m-$1-m)"

main :: IO ()
main = do 
   p <- newPureMT
   let str= randString p
   --defaultMain [ bench "replace big file" $ whnf transf (take 10000000 str) ]
   defaultMain [ bench "write random" $ nfIO writeRandom ]

writeRandom :: IO ()
writeRandom = do
   p <- newPureMT
   let str= randString p
   writeFile "bigfile.txt"  $ transf (take 100000000 str)
   putStrLn $ "Written bigfile.txt"
   return ()
