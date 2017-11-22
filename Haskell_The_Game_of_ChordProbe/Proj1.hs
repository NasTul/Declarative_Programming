--  File     : Proj1.hs
--  Author   : Xiangyu Gao
--  Origin   : Tue Sep 05 10:20:00 2017
--  Purpose  : A ChordProbe game for guessing the right three-pitch music 
--             chord.
--
-- |This code is a implementation of guessing the right chords by given an 
-- initial guess. The test file will give a target which is unknown and what 
-- this code does is using the initial guess and the feedback from the 
-- previous guess to determine the next guess. This process will be called 
-- repeatedly until it produces the correct guess.

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Function

-- | The GameState maintains the list of all the possible three-pitch chord 
-- and each combination of chords will be a list of String.
type GameState = [[String]]

-- | Generate all 21 pitches and each pitch is represented as a list of 
-- two-character strings, where the first character is Note ranging from 
-- 'A' to 'G', and the second is Octave between '1' and '3'.
pitch :: [String]
pitch = [note:octave:[] | note <- ['A'..'G'], octave <- ['1', '2', '3']]

-- | Generate all 1330 possible chord first by selecting three pitches as a 
-- collection and no pitch may appear more than once.
chord :: GameState
chord = [[p1,p2,p3] | 
    p1 <- sort(pitch), 
    p2 <- sort(pitch), 
    p3 <- sort(pitch),
    p2 > p1, 
    p3 > p2]

-- | The initial guess will be ["A1","B1","C2"] and it can be more efficient 
-- after the first guess.
initialGuess :: ([String], GameState)
initialGuess = (firstGuess,initialGameState)
    where firstGuess       = ["A1","B1","C2"]
          initialGameState = chord

-- | Generate the next guess according to the previous guess and the feedback 
-- from the that guess.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (prev, gameState) result = (next, newGameState)
    where tempGameState = possibleTargets (prev, gameState) result
          next          = maxPossibleTargets tempGameState
          newGameState  = delete next tempGameState

-- | Compute the feedback to a guess. The feedback is a 3-tuple where the 
-- first indicates the correct pitches, the second is correct notes and the 
-- last one is correct octaves.
answer :: [String] -> [String] -> (Int, Int, Int)
answer target guess = (correctPitch, correctNote, correctOctave)
    where common = intersect target guess
          correctPitch  = length common
          targetNote    = map head $ filter (`notElem` common) target
          guessNote     = map head $ filter (`notElem` common) guess
          targetOctave  = map last $ filter (`notElem` common) target
          guessOctave   = map last $ filter (`notElem` common) guess
          correctNote   =  compNoteAndOctave guessNote targetNote  
          correctOctave =  compNoteAndOctave guessOctave targetOctave

-- | Compute how many Notes and Octaves are same.
compNoteAndOctave :: String -> String -> Int
compNoteAndOctave _ [] = 0
compNoteAndOctave [] _ = 0
compNoteAndOctave (x:xs) ys
    | x `elem` ys = 1 + compNoteAndOctave xs (delete x ys)
    | otherwise   = compNoteAndOctave xs ys

-- | Screen out all the consistent guesses that received for previous guesses 
-- in GameState.
possibleTargets :: ([String], GameState) -> (Int, Int, Int) -> GameState
possibleTargets (guess, gameState) result 
    = filter (\x -> answer guess x == result) gameState

-- | Compute the all the combination of possible answers which has the same 
-- output answer in the specific guess.
outputSet :: [String] -> GameState -> [[(Int, Int, Int)]]
outputSet guess gameState = 
    let candidateAnswers  = map (\x -> answer guess x) gameState
        sortedAnswers     = sort candidateAnswers
    in  group sortedAnswers 

-- | Compute the expected number of remaining possible targets for each guess.
avgRemainingCandidates :: [String] -> GameState -> ([String], Float)
avgRemainingCandidates guess gameState = (guess, expected)
    where countOutput = map length $ outputSet guess gameState
          sumOutput   = sum countOutput
          expected    = foldr (\x acc -> 
            fromIntegral x^2 / fromIntegral sumOutput + acc) 0 countOutput

-- | Choose the guess with the smallest expected number of remaining possible 
-- targets and take it as the next guess.
maxPossibleTargets :: GameState -> [String]
maxPossibleTargets gameState = 
    let collection = map (\x -> avgRemainingCandidates x gameState) gameState
    in fst $ minimumBy (compare `on` snd) collection
