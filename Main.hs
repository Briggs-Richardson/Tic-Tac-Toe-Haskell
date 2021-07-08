{- Name: Briggs Richardson
 - Date: Mar 2021
 - File: Main.hs - defines the IO interaction with the user. The Main module
 - is responsible for getting input from the user and defining the sequences
 - of expressions to manage the game state
-}
module Main where

import System.IO (putStrLn, getLine)

import Game
import Graphics (showGame)

-- difficulty names --
easy = "1" :: String
normal = "2" :: String
impossible = "3" :: String

-- main function. Entry point for IO interaction
main = do
  putStrLn "-------------"
  putStrLn " Tic Tac Toe "
  putStrLn "-------------"
  difficulty <- getDifficultyLevel
  let game = initializeGame Human
  playGame game difficulty 


-- Sequence of expressions to handle human's turn
playGame :: Game -> String -> IO ()
playGame game@Game{turn=Human} difficulty = do
  putStrLn "Current State of Game"
  showGame game  
  userMove <- getMove game "Enter your move (1-9)" 
  let updatedGame = markMove game userMove 
  putStrLn "\nUpdated State of Game"
  showGame updatedGame
  putStrLn "Press enter to continue"         -- Let the user advance to next move
  getLine                                    -- when ready
  putStrLn "---------------------------------"
  if (state updatedGame) == Active
    then 
      playGame updatedGame difficulty
    else do
      putStrLn "Final Game State"
      showGame updatedGame 
      printGameResult updatedGame


-- Sequence of expressions to handle computer's turn
playGame game@Game{turn=Computer} difficulty = do
  let randomComputerPos = chooseOnDifficulty game difficulty 
  let updatedGame = markMove game randomComputerPos
  putStrLn "Computer's Turn!"
  putStrLn $ "The Computer chose to mark position " ++ (show randomComputerPos)
  putStrLn "---------------------------------"
  if (state updatedGame) == Active
    then playGame updatedGame difficulty
    else do
      putStrLn "Final Game State"
      showGame updatedGame 
      printGameResult updatedGame

-- Based on the difficulty chosen by the user, select a move via the minimax
-- algorithm on increasing depth levels 
chooseOnDifficulty :: Game -> String -> Int
chooseOnDifficulty game difficulty 
  | difficulty == easy   = fst $ minimax game 1 0 Computer
  | difficulty == normal = fst $ minimax game 4 0 Computer
  | otherwise            = fst $ minimax game 9 0 Computer


-- Gets a valid legal move from the user 
getMove :: Game -> String -> IO Int
getMove game prompt 
  | null (availableMoves game) = error "Error! No valid moves left." 
  | otherwise = do
      putStrLn prompt  
      input <- getLine
      let pos = read input :: Int
      if isLegalMove game pos
        then return pos
        else do
          putStrLn "Invalid Move. Please try again"
          getMove game prompt


-- Get the preferred difficulty level from user
getDifficultyLevel :: IO String
getDifficultyLevel = do
  putStrLn "\nPlease enter a difficulty level"
  putStrLn "1) Easy"
  putStrLn "2) Normal"
  putStrLn "3) Impossible"
  input <- getLine
  if input /= "1" && input /= "2" && input /= "3" 
    then do 
      putStrLn "Invalid selection. Please try again."
      getDifficultyLevel
    else do 
      putStrLn "" -- blank line return input
      return input


printGameResult :: Game -> IO ()
printGameResult game 
  | (state game) == HumanWon    = putStrLn "You won! Congratulations!"
  | (state game) == ComputerWon = putStrLn "The computer won! Better luck next time!"
  | otherwise                   = putStrLn "Tie game!"
