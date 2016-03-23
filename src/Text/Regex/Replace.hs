{-#LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies  #-}
module Text.Regex.Replace (
    replaceAll
   ,replaceFirst
   ,replace
   ,excise
) where

import Text.Regex.Base
import Data.Array
import Control.Monad
import Control.Monad.Identity
import Control.Arrow (first,app)
import Text.Parsec
import Data.String

replaceAll :: (Eq a, IsString a, Monoid a, Extract a, RegexLike r a, Stream s Identity Char) => a -> r -> s -> a
replaceAll a r s = loop_ (mappend mempty,a)
   where rs = parseReplacement s
         loop_ (h,t) = if t == mempty
                       then h mempty
                       else loop_ $ first ((.) h ) $ replaceStep t r rs

replaceFirst :: (IsString a, Monoid a, Extract a, RegexLike r a, Stream s Identity Char) => a -> r -> s -> a
replaceFirst a r s = app $ replaceStep a r (parseReplacement s)


replace :: (IsString a, Monoid a, Extract a, RegexLike r a, Stream s Identity Char) => a -> r -> s -> a
replace a r s = doreplace a (matchAll r a) s

excise :: (IsString a, Monoid a, Extract a, RegexLike r a) => a -> r -> a
excise a r = doexcise a (matchAll r a)

replaceStep :: (IsString a, Monoid a, Extract a, RegexLike r a) => a -> r -> [Replacement a] -> (a -> a,a)
replaceStep a r rs = case (matchOnce r a) of
                     Just(m) -> if (matchLen m) == 0
                                    then (mappend a, mempty)
                                    else let offs = matchOffs m
                                             tot  = offs + (matchLen m)
                                             rpl  = bFuncLstApp rs a m 
                                         in (mappend (before offs a) . mappend rpl, after tot a)
                     Nothing -> (mappend a, mempty)

bFuncLstApp :: (Monoid c) => [a->b->c] -> a -> b -> c
bFuncLstApp fs a b = mconcat $ map (\f -> f a b) fs

doreplace :: (IsString a, Monoid a, Extract a, Stream s Identity Char) => a -> [MatchArray] -> s -> a 
doreplace a ms s = prc ms
   where prc []  = a
         prc ms' = mconcat $ repl $ finl $ extr $ foldl twist ([],0) ms'
         twist (as,sm) m = ((sm,(offs m)-sm):as,(offs m)+(len m))
         offs m  = fst (m ! 0)
         len  m  = snd (m ! 0)
         extr (is,l) = (map (flip extract $ a) is, l)
         finl (ex,l) = (after l a):ex
         repl  []    = []
         repl  (p:ps)= case (runParser replacement () "Replacement String" s) of
                        Right rs  -> (snd $ foldl (dorep rs) (reverse ms,[]) ps) ++ [p]
                        Left  err -> []
         dorep rs (m:ms',ss) p = (ms',p:(mconcat $ map (\f -> f a m) rs):ss)

doexcise :: (Monoid s, Extract s) => s -> [MatchArray] -> s 
doexcise s [] = s
doexcise s ms = mconcat $ reverse $ tailExtract s $ incrListExtract s $ nonMatchingParts ms

type Replacement a = a -> MatchArray -> a

parseReplacement :: (IsString a, Extract a, Stream s Identity Char) => s -> [Replacement a]
parseReplacement s = case (runParser replacement () "Replacement String" s) of
                        Right rs ->  rs
                        Left err -> fail $ "Error in replacement string: " ++ (show err)

replacement :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m [Replacement a]
replacement = many (part <|> mgroup <|> dollar)

part :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
part = many1 (noneOf "$") >>= constR 

mgroup :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
mgroup = try (char '$' >> fmap read (many1 digit) >>= subgroupR)

dollar :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
dollar = char '$' >> char '$' >> constR "$"

constR :: (IsString a, Extract a, Stream s m Char) => String -> ParsecT s u m (Replacement a)
constR s = return $ \ _ _ -> (fromString s)

subgroupR :: (IsString a, Extract a, Stream s m Char) => Int -> ParsecT s u m (Replacement a)
subgroupR i= return $ \s m -> extract (m ! i) s


-- | Extracts the non matching tail of the matching source @s@.
--   This is a helper function applicable to the output of @incrListExtract@.
tailExtract :: (Monoid s, Extract s) => s -> ([s],Int) -> [s]
tailExtract s (as,l)= (after l s):as

-- | Extracts all non-matching parts but the last one using the computed tuple 
--   from @nonMatchingParts@. This also returns a tuple so that @nonMatchingTail@ is able
--   to compute the non matching tail of the sequence.
incrListExtract :: (Monoid s, Extract s) => s -> ([(Int,Int)],Int) -> ([s],Int)
incrListExtract s (as,lastOffs) = (map (flip extract $ s) as, lastOffs)

-- | Convert a List of MatchArray into a tuple containing a list of Offset-Length pairs
--   for extraction of non matching parts of a string and a final offset for the nonmatching
--   tail. The second part of the tuple is 0 when the last match is at the end of the string.
nonMatchingParts :: [MatchArray] -> ([(Int,Int)],Int)
nonMatchingParts = foldl matchConvertStep ([],0)

-- | Helper function to convert a MatchArray List into a List of Offsets and 
--   Lengths for extraction of the non-matching parts
matchConvertStep :: ([(Int,Int)],Int) -> MatchArray -> ([(Int,Int)],Int)
matchConvertStep (as,nextOffs) m = ((nextOffs, (matchOffs m)-nextOffs):as,(matchOffs m)+(matchLen m))

-- | Convenience function: return the match offset of the complete match given 
--   a MatchArray
matchOffs :: MatchArray -> Int
matchOffs = fst . (flip (!) $ 0) 

-- | Convenience function: return the match length of the complete match given 
--   a MatchArray
matchLen :: MatchArray -> Int
matchLen = snd . (flip (!) $ 0)
