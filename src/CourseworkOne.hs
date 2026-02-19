module CourseworkOne where

import Backwords.Types
import qualified Data.Map as Map
import Backwords.WordList

import Data.Maybe
import Data.List 
import Data.Char
import Data.Ratio


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

- To create a display instance for Char, I just had to implement the display function from the class.
- The function takes in one character as a variable, and returns the upper case character in a box as required by the specification
-}

instance Display Char where
    display :: Char -> String
    display x = "+---+\n| " ++ [toUpper x] ++ " |\n+---+"

{- Ex. 2 - Give a Display instance for [Char]

- Once again I have to implement the display function from the class, but this time for a String instead of a singular Char.
- Since the upper part of the boxes, the characters themselves and the lower part of the boxes are drawn on sepearate lines I created two helper functions, bounds and characters, which draw each aspect of the final string, before the display function concatenates them all together.
- The helper functions use pattern matching to decide on whether to add a space to the end of a string or not.

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

- Here I simply checked defined a function isValidWord which takes in a String and determinse whether the word expressed by that string is an element of the allWords list. 
- Because all elements of the allWords list are lower case, the toLower function must be mapped to each character of the String.

-}

isValidWord :: String -> Bool
isValidWord x = map toLower x `elem` allWords

{- Ex. 4 - Determine the points value of a letter

- Each letter corresponds to a predifined value. By checking ownership of characters within a string, we can map each letter to its corresponding value.


-}
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

{- Ex. 5 - Score a word according to the backwords scoring system

- For any given character in a string, the total score of the word from the end to that character is equal to the score of the rest of the word up to that character * 2, plus the value of the character itself. 
- This scoring function can be implemented elegently using a fold

-}

scoreWord :: String -> Int
scoreWord = foldr ((\y z -> y + 2 * z) . letterValue) 0

{- Ex. 6 - Get all words that can be formed from the given letters

- My approach here is to 'iterate' through the entire list of words and check which ones can be formed by the given letters, one by one
- To help with this, I created a function canBeFormedFrom which determines whether a string can be formed using the characters in another string. 
- The possibleWords function simply uses the above function to filter out the desired words

-}


canBeFormedFrom :: String -> String -> Bool
canBeFormedFrom y [] = True
canBeFormedFrom y (x:xs)
                | x `elem` y = canBeFormedFrom (y\\[x]) xs
                | otherwise = False


possibleWords :: [Char] -> [String]
possibleWords x = filter (canBeFormedFrom x) allWords


{- Ex. 7 - Given a set of letters, find the highest scoring word that can be formed from them.

- The idea here is to extract the word which yields the maximum score. This can be done by calling maximumBy on possibleWords x, where the sorting key is the score of the words.
- If the list is empty, then the function should return nothing

-}

bestWord :: [Char] -> Maybe String
bestWord x
        | null (possibleWords x) = Nothing
        | otherwise = Just (maximumBy (\x y -> compare (scoreWord x) (scoreWord y)) (possibleWords x))



{- Ex. 8 - Given a list of letters, and a word, mark as used all letters in the list that appear in the word 

- The idea here is simlar to the idea implemented in canBeFormedBy in Ex.6. We 'iterate' through the list, and every time a tile is found in the string, the tile is marked as used, and the rest of the word excluding that letter is checked.


-}

useTiles :: [Char] -> String -> [Tile]
useTiles x [] = [Unused y | y <- x]
useTiles (x:xs) y
            | x `elem` y = Used x : useTiles xs (y\\[x])
            | otherwise = Unused x : useTiles xs y

{- Ex. 9 - Given a nonempty bag of possible letters as a list, return the chance of drawing each letter.

- To calculate the distribution I created a uniqueLetters and countOccurances function:
- Given a string, the uniqueLetters function returns a list containing all the unique letters appearing in the string
- The count occurances function counts how many times a given letter appears in a string, returning a pair of the character and the number of times it appeared.
- By applying the countOccurances function to every character given by uniqueLetters, the bagDistribtution function calculates the probabilty of drawing each letter by diving the number of occurances by the length of the possible letters list


-}
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

{- Ex. 10 - Write an AI which plays the Backwords game as well as possible

- The basic idea is for the AI to draw the tiles which on average generate more points, and then play the best word at every opportunity.
- The AI is restricted to having a maximum of 6 consonants and 4 vowels on the board - I found that this maximises the score. 
- A number of helper functions are defined here: 
 - containsVowel and containsConsonant: Determines whether a string contains a vowel or consonant respectively
 - isVowel and isConsonant: If the given character is a vowel or consonant respectively, return 1, else return 0.
 - countVowels and countConsonants: count the number of vowels or consonants respectively in a string, by folding using the functions above
 - expectedValue: A heuristic function which judges the worth of picking up a tile from a given pool of characters. It looks at the average number of points obtained weighted by the probability distribution of the bag. 
 - bagPickUp: Decides when to pick a vowel and when to pick up a consonant.

- Finally, aiMove handles whether the AI plays a word, or picks up a tile 

- Overall, I found the AI to average around 2500-2600 points per game.

-}

containsVowel :: String -> Bool
containsVowel = any (`elem` vowels)

containsConsonant :: String -> Bool
containsConsonant = any (`elem` consonants)

isVowel :: Char -> Integer
isVowel x
        | x `elem` vowels = 1
        | otherwise = 0

isConsonant:: Char -> Integer
isConsonant x
        | x `elem` consonants = 1
        | otherwise = 0

countVowels :: String -> Integer
countVowels = foldr (\x y -> isVowel x + y) 0 

countConsonants :: String -> Integer
countConsonants = foldr (\x y -> isConsonant x + y) 0


expectedValue bag rack collection = let
                        probs = Map.fromList (bagDistribution bag)
                        testLetter x = toRational (scoreWord(fromMaybe "" (bestWord (rack++[x])))) * fromMaybe 0 (Map.lookup x probs)
                        temp = [testLetter y | y <- collection, y `elem` bag]
                in 
                        sum temp / toRational (length temp)

bagPickUp :: [Char] -> [Char] -> Move
bagPickUp bag rack 
                | countVowels rack >= 4 && containsConsonant bag = TakeConsonant
                | countConsonants rack >= 6 && containsVowel bag = TakeVowel
                | containsVowel bag && containsConsonant bag && (expectedValue bag rack consonants > expectedValue bag rack vowels * 0.6) = TakeConsonant
                | containsVowel bag && containsConsonant bag && (expectedValue bag rack vowels * 0.6 >= expectedValue bag rack consonants) = TakeVowel
                | containsConsonant bag = TakeConsonant
                | containsVowel bag = TakeVowel
                | otherwise = TakeVowel



aiMove :: [Char] -> [Char] -> Move
aiMove bag rack
        | length rack == 9 = PlayWord (fromMaybe "" (bestWord rack))
        | otherwise = bagPickUp bag rack

