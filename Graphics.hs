{- Name: Briggs Richardson
 - Date: March 2021
 - File: Graphics.hs - The Graphics Module is entirely responsible for the 
 - showGame function. A function that prints out the board state of the game
 - to the console for the user
-}
module Graphics (showGame) where

import Game
import Data.List

-- Print the board to IO as a 2D 3x3 square grid
showGame :: Game -> IO ()
showGame game = do
  putStrLn ""          -- extra blank line on top
  putStrLn $ renderRow topRow
  putStrLn renderHorizontalLine 
  putStrLn $ renderRow middleRow
  putStrLn renderHorizontalLine
  putStrLn $ renderRow bottomRow
  putStrLn ""         -- extra blank line on bottom
  where gridContents = gridToString $ board game
        topRow = slice gridContents 0 2
        middleRow = slice gridContents 3 5
        bottomRow = slice gridContents 6 8


-- Renders a row given a [Char] where each Char represents the content of one cell
-- ex. ['X','O','3'] to X | O | 3 
renderRow :: String -> String
renderRow row = " " ++ (intersperse ' ' ( intersperse '|' row ))


-- Renders a horizontal line to seperate grid rows. 
-- 3 dashes for each cell, 1 dash for each vertical | = 11 total dashes
renderHorizontalLine :: String
renderHorizontalLine = replicate 11 '-'


-- Given a list of Cells, return it expressed as a string of its Contents
-- as displayed by Show
gridToString :: Board -> String
gridToString grid = [x | x <- (show grid), x /= '[', x /= ']', x /= ',']
