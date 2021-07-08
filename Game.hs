{- Name: Briggs Richardson
 - Date: Mar 2021
 - File: Game.hs - The Game module is responsible for the data definitions of
 - what a Game is. It contains functions for producing Game states for
 - specific scenarios. 
 - 1) Initializing a empty game state 
 - 2) Given a game state, create a new game state with an additional inputted move
 - 3) Evaluate a game state and return information about it 
 -    such as determining if the GameState has changed, extracting part of
 -    the board, or determining the best move
-}
module Game where

import Data.List (sum, delete, minimumBy, maximumBy)
import Data.Ord (comparing)

type Board = [Cell]

data Player = Human | Computer deriving (Show, Eq)
data Cell = Occupied Player | UnOccupied Int deriving (Eq)
data GameState = Active | Tie | HumanWon | ComputerWon deriving (Show, Eq)

data Game = Game { board :: Board
                 , turn :: Player
                 , availableMoves :: [Int]
                 , state :: GameState
                 } deriving (Show)

instance Show Cell where
  show (Occupied player) = if player == Human then "X" else "O"
  show (UnOccupied position) = show position


-- Initialize and return a starting game state where the given player goes first
initializeGame :: Player -> Game
initializeGame firstPlayer = Game initialBoard firstPlayer [1..9] Active 


-- Initialize and return a fully UnOccupied list to represent the starting board state
initialBoard :: Board
initialBoard = [UnOccupied cellNum | i <- [1..9], let cellNum = fromIntegral i]


{- Given a Game state and a desired move, create and return a new Game state with the 
   move marked and all other game fields updated
-}
markMove :: Game -> Int -> Game
markMove game move = Game newBoard togglePlayer 
  newAvailMoves (updateState newBoard newAvailMoves (turn game) move)
  where newBoard = setCell (board game) move (Occupied $ turn game)
        newAvailMoves = delete move (availableMoves game)
        togglePlayer = if (turn game) == Human then Computer else Human


{- Create a new board with the given position marked with the Cell to represent
   a new Board state with a new move marked
-}
setCell :: Board -> Int -> Cell -> Board
setCell [] _ _ = []
setCell (x:xs) 1 newCell = newCell : xs 
setCell (x:xs) pos newCell = x : setCell xs (pos - 1) newCell


-- Determines if the position selected for the current game state is valid
isLegalMove :: Game -> Int -> Bool
isLegalMove game pos
  | pos < 1 || pos > 9  = False
  | otherwise                     = pos `elem` (availableMoves game)
  where index = pos - 1


{- Given the last player who went, and the last move marked, determine
   if that specific move changed the state of the game
   Note: We're using pattern matching, 1 function definition for the Human
   and 1 function definition for the Computer. We do this to save computation.
   The Player who last went is either capable of drawing the game or winning
   it for themselves. The other Player cannot win if it's not their turn.
-}
updateState :: Board -> [Int] -> Player -> Int -> GameState
updateState board availMoves Human lastMove
  | hasWon board lastMove         = HumanWon
  | null availMoves               = Tie
  | otherwise                     = Active

updateState board availMoves Computer lastMove
  | hasWon board lastMove         = ComputerWon
  | null availMoves               = Tie
  | otherwise                     = Active


{- Given that the board is 3x3 set in stone, we can manually check all the 
   possible lines of 3 cells to determine if any Player has won. If there's
   a winner, return True, else return False
-}
hasWon :: Board -> Int -> Bool
hasWon board pos = or allPossibleLines
  where allPossibleLines = [row board pos, col board pos, diag board pos]
        row list pos
          | pos <= 3       = allEqual $ slice list 0 2
          | pos <= 6       = allEqual $ slice list 3 5
          | otherwise      = allEqual $ slice list 6 8
        col list pos = allEqual $ getColumn list pos 
        diag list pos
          | even pos  = False
          | pos == 3 || pos == 7    = allEqual [list !! 2, list !! 4, list !! 6]
          | pos == 1 || pos == 9    = allEqual [list !! 0, list !! 4, list !! 8]
          | otherwise               = or [allEqual [list !! 2, list !! 4, list !! 6],
                        allEqual [list !! 0, list !! 4, list !! 8]]


-- Extracts a slice of a list between the range of two indecies
slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) (drop start list)

-- Extracts the pos's column out of a 1D list representing a 3x3 grid
getColumn :: [a] -> Int -> [a]
getColumn list pos = [list !! x, list !! (x+3), list !! (x+6)]
  where x = (pos - 1) `mod` 3

-- Returns true if all elements in the given list are equal
allEqual :: (Eq a) => [a] -> Bool
allEqual (x:xs) = all (==x) xs


----- Computer Generated Move ------

{- We use the minimax algorithm to think some number of moves ahead and 
 - return the pair of the (a,b) where a is the best move, and b is the 
 - evaluated score for that move based on the current game state
 -
 - minimax : We pass as a parameter the move :: Int, which tells us the move
 - taken to get to the current game state. We need this information so we can
 - return the move that takes us to the best game state 
-}

minimax :: (Eq a, Num a) => Game -> a -> Int -> Player -> (Int, Double)

-- Base case -- We have reached depth 0, return the static evaluation of game state
minimax game 0 move _ = (move, evaluateGame game)


-- Consider Computer moves (maximizing player), and return max value move
minimax game depth move Computer 
  | null (availableMoves game)  = (move, evaluateGame game) -- special case : no moves to consider
  | (state game) /= Active      = (move, evaluateGame game) -- special case : game is over
  | otherwise =
     maximumBy (comparing snd) [(a, score) | a <- (availableMoves game), 
      let (_,score) = minimax (markMove game a) (depth-1) a Human]

-- Consider Human moves (minimizing player), and return min value move
minimax game depth move Human  
  | null (availableMoves game)  = (move, evaluateGame game) -- special case : no moves to consider
  | (state game) /= Active      = (move, evaluateGame game) -- special case : game is over
  | otherwise = 
     minimumBy (comparing snd) [(a, score) | a <- (availableMoves game), 
      let (_,score) = minimax (markMove game a) (depth-1) a Computer]


{- Evluate and score a given game state for the minimax algorithm
 - The computer is the maximizing player, the human is the minimizing player
 - If either player won, return the MAX/MIN value of an Int (as winning is the best
 - possible state for either player)
 - otherwise return the sum based on positions the players are in, where
 - corners and middle have 1.5 multipliers
-}
evaluateGame :: Game -> Double
evaluateGame game 
  | (state game) == HumanWon     = (-10000000.0) -- Hacky solution for there
  | (state game) == ComputerWon  = 10000000.0    -- being no min/max Double
  | otherwise = sum $ getPoints (board game)
  where getPoints board = [if even x then 1.5 * p else p | x <- [0..8], let p = getCell board x]
        getCell board x 
          | board !! x == (UnOccupied (x+1)) = 0
          | board !! x == (Occupied Human)   = (-1)
          | otherwise                        = 1
