module CourseworkOne where

import Backwords.Types
import Backwords.WordList

import Data.List 
import Data.Char
import Data.Ratio
import Data.Bits (Bits(xor))
import Text.ParserCombinators.ReadP (count)
import Data.Either (partitionEithers)
import Data.Data (Data(dataCast1))

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: 5686298
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

{- Ex. 1 - Give a Display instance for Char

To create a display instance for Char, I just had to implement the display function from the class.
The function takes in one character as a variable, and returns the upper case character in a box as required by the specification
-}

instance Display Char where
    display :: Char -> String
    display x = "+---+\n| " ++ [toUpper x] ++ " |\n+---+"

{- Ex. 2 - Give a Display instance for [Char]

Once again I have to implement the display function from the class, but this time for a String instead of a singular Char.
Since the upper part of the boxes, the characters themselves and the lower part of the boxes are drawn on sepearate lines I created two helper functions, bounds and characters, which draw each aspect of the final string, before the display function concatenates them all together.
The helper functions use pattern matching to decide on whether to add a space to the end of a string or not.

-}

instance Display String where
    display :: String -> String
    display [] = []
    display x = bounds x ++ "\n" ++ characters x ++ "\n" ++ bounds x where 
        bounds :: String -> String
        bounds [] = []
        bounds [x] = "+---+"
        bounds (x:xs) = "+---+ " ++ bounds xs

        characters :: String -> String
        characters [] = []
        characters [x] = "| " ++ [toUpper x] ++ " |"
        characters (x:xs) = "| " ++ [toUpper x] ++ " | " ++ characters xs

{- Ex. 3 - Determine if a word is valid

Here I simply checked defined a function isValidWord which takes in a String and determinse whether the word expressed by that string is an element of the allWords list. 
Because all elements of the allWords list are lower case, the toLower function must be mapped to each character of the String.

-}

isValidWord :: String -> Bool
isValidWord x = map toLower x `elem` allWords

{- Ex. 4 - Determining the points value of a letter

Each letter corresponds to a predifined 
-}

-- Ex. 4:
-- Determine the points value of a letter.
letterValue :: Char -> Int
letterValue x
        | toLower x `elem` "aeilnorstu" = 1
        | toLower x `elem` "dg" = 2
        | toLower x `elem` "bcmp" = 3 
        | toLower x `elem` "fhvwy" = 4
        | toLower x `elem` "k" = 5
        | toLower x `elem` "jx" = 8
        | toLower x `elem` "qz" = 10
        | otherwise = 0

-- Ex. 5:
-- Score a word according to the Backwords scoring system.
scoreWord :: String -> Int
scoreWord = foldr ((\y z -> y + 2 * z) . letterValue) 0

-- Ex. 6:
-- Get all words that can be formed from the given letters.

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = [x:y | y <- k] ++ k where 
    k = subsets xs

possibleWords :: [Char] -> [String]
possibleWords x = [ y | y <- allWords, sort y `elem` k] where 
    k = subsets (sort x)

-- Ex. 7:
-- Given a set of letters, find the highest scoring word that can be formed from them.

bestWord :: [Char] -> Maybe String
bestWord x
        | null (possibleWords x) = Nothing
        | otherwise = Just (maximumBy (\x y -> compare (scoreWord x) (scoreWord y)) (possibleWords x))



-- Ex. 8:
-- Given a list of letters, and a word, mark as used all letters in the list that appear in the word.
useTiles :: [Char] -> String -> [Tile]
useTiles x [] = [Unused y | y <- x]
useTiles (x:xs) y
            | x `elem` y = Used x : useTiles xs (y\\[x])
            | otherwise = Unused x : useTiles xs y

-- Ex. 9:
-- Given a nonempty bag of possible letters as a list, return the chance of drawing 
-- each letter.
uniqueLetters :: [Char] -> [Char]
uniqueLetters [] = []
uniqueLetters (x:xs)
                | x `elem` k = k
                | otherwise = x:k
                where
                    k = uniqueLetters xs

countOccurances :: [Char] -> Char -> (Char, Integer)
countOccurances x y = (y, toInteger (length (filter (==y) x)))

bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution x = [ (a,b % toInteger (length x)) | (a,b) <- map (countOccurances x) (uniqueLetters x)]

-- Ex. 10:
-- Write an AI which plays the Backwords game as well as possible.

extractJust :: Maybe String -> String
extractJust (Just x) = x
extractJust Nothing = ""

containsVowel :: String -> Bool
containsVowel = any (`elem` "aeiou")

isVowel :: Char -> Integer
isVowel x
        | x `elem` "aeiou" = 1
        | otherwise = 0

isConsonant:: Char -> Integer
isConsonant x
        | x `elem`"bcdfghjklmnpqrstvwxyz" = 1
        | otherwise = 0

f :: Char -> Integer -> Integer
f a b = isVowel a + b

g :: Char -> Integer -> Integer
g a b = isConsonant a + b

countVowels :: String -> Integer
countVowels = foldr f 0 

countConsonants :: String -> Integer
countConsonants = foldr g 0

containsConsonant :: String -> Bool
containsConsonant = any (`elem` "bcdfghjklmnpqrstvwxyz")

cheapLetters rack n
            | potentialWord == Nothing = cheapLetters rack (n+1)
            | otherwise = extractJust potentialWord where 
                potentialWord = bestWord [x | x <- rack, letterValue x <= n]

{-
bagPickUp :: [Char] -> Move
bagPickUp bag
        | length bag `mod` 2 == 0 && containsVowel bag = TakeVowel 
        | length bag `mod` 2 /= 0 && containsConsonant bag = TakeConsonant
        | containsConsonant bag = TakeConsonant
        | containsVowel bag = TakeVowel
        | otherwise = TakeConsonant
-}

bagPickUp :: [Char] -> [Char] -> Move
bagPickUp bag rack
        | (length bag `mod` 5 == 0) && containsVowel bag = TakeVowel
        | (countVowels rack < 3) && containsVowel bag = TakeVowel
        | (countConsonants rack < 6) && containsConsonant bag = TakeConsonant
        | containsVowel bag = TakeVowel
        | containsConsonant bag = TakeConsonant 
        | otherwise = TakeConsonant

rackPlay rack
--            | (scoreWord potentialWord <= 30) || (length potentialWord <= 3) = PlayWord (cheapLetters rack 5)
            | otherwise = PlayWord potentialWord
            where
                potentialWord = extractJust (bestWord rack)




aiMove :: [Char] -> [Char] -> Move
aiMove bag rack
        | length rack == 9 = rackPlay rack
--        | (length rack /= 9) && null bag = error "tf am i supposed to do here"
        | (length rack /= 9) && (length bag < 0) = PlayWord (extractJust (bestWord rack))
        | otherwise = bagPickUp bag rack

