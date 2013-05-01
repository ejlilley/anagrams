module Anagrams 
       (makeAnag, removeTooBig, searchAnag, uniq, first, joinS, normalise, subsetVector) 
       where

import Data.Char
import Data.List
import Data.Int
import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T

data WordVector = WVec (V.Vector Int8) deriving (Eq, Ord)

instance Show WordVector where
  show (WVec v) = "\n" ++ (show v)

type Anag = (WordVector, [String])

joinS :: [a] -> [[a]] -> [a]
joinS d [] = []
joinS d (x:[]) = x
joinS d (x:xs) = x ++ d ++ joinS d xs

alphabet = ['a'..'z']

replacements = zip ['A'..'Z'] ['a'..'z']

findReplacement [] c = c
findReplacement (r:rs) c = if fst r == c
                           then snd r
                           else findReplacement rs c

strip [] c = Nothing
strip (a:as) c = if a == c
                 then Just c
                 else strip as c

removeNothings [] = []
removeNothings (Just x:xs) = x : removeNothings xs
removeNothings (Nothing:xs) = removeNothings xs

normalise :: String -> String
normalise = removeNothings . (map ((strip alphabet) . (findReplacement replacements)))

findWith eq c l = or $ map (eq c) l

foundIn :: Eq a => a -> [a] -> Bool
foundIn = findWith (==)

uniq :: Eq a => [a] -> [a]
uniq = reverse . (u [])
  where u seen [] = seen
        u seen (x:xs) = if x `foundIn` seen
                        then u seen xs
                        else u (x:seen) xs

countChars c [] = 0
countChars c (x:xs) = if c == x
                      then 1 + countChars c xs
                      else countChars c xs

makeVector :: String -> WordVector
makeVector s = let offset = 97 :: Int
                   l = 26 :: Int
               in WVec (V.generate l (\c -> fromIntegral (countChars (chr (c + offset)) s)))
                  
makeAnag :: String -> Anag
makeAnag s = (makeVector (normalise s), sort [s])

addAnag :: Anag -> Anag -> Anag
addAnag s1 s2 = (addVector (fst s1) (fst s2), sort ((snd s1) ++ (snd s2)))

vectorOp op (WVec a) (WVec b) = WVec (V.zipWith op a b)

addVector = vectorOp (+)
subVector = vectorOp (-)

intOp :: (Int8 -> Int8 -> Bool) -> Int8 -> Int8 -> Int8
intOp op a b = if op a b
               then 1
               else 0
                    
intEq = intOp (==)
intLe = intOp (<=)

ones = makeVector alphabet
zeroes = makeVector ""

-- allOnes (WVec v) = (V.sum v) == 26
allOnes = (==) ones
-- allZeroes (WVec v) = (V.sum v) == 0
allZeroes = (==) zeroes

zeroVector = allZeroes

-- eqVector a b = allOnes $ vectorOp intEq a b
eqVector = (==)
subsetVector a b = allOnes $ vectorOp intLe a b


sameLetters :: Anag -> Anag -> Bool
sameLetters a1 a2 = eqVector (fst a1) (fst a2)
notSameLetters a1 a2 = not (sameLetters a1 a2)


first p [] = Nothing
first p (x:xs) = if p x
                 then Just x
                 else first p xs

removeTooBig :: WordVector -> [Anag] -> [Anag]
removeTooBig goal candidates = filter (\x -> subsetVector (fst x) goal) candidates


-- -- A working, but super-slow algorithm:
-- prod :: [Anag] -> [Anag] -> [Anag]
-- prod xs ys = [addAnag x y | x <- xs, y <- ys]

-- search :: Anag -> [Anag] -> [Anag] -> [Anag]
-- search _ _ [] = []
-- search _ [] _ = []
-- search goal dict candidates = let filtered = uniq $ removeTooBig (fst goal) candidates
                                  -- success = filter (sameLetters goal) filtered
                                  -- failed = filter (notSameLetters goal) filtered
                              -- in success ++ search goal dict (prod failed dict)


searchAnag :: Anag -> [Anag] -> [[String]]
searchAnag goal dict = search (fst goal) dict []

search :: WordVector -> [Anag] -> [String] -> [[String]]

search _ [] [] = []
search g [] result = if zeroVector g
                     then [result]
                     else []

search g dict@((v,w):ws) result =
  if zeroVector g
  then [result]
  else if v `subsetVector` g
       then search (g `subVector` v) dict (w ++ result)
            ++ search g ws result
       else search g ws result

