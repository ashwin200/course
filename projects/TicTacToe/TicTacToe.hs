module TicTacToe where


import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

data Player = Naught | Cross
	deriving (Eq, Show)

data Board =  
	Board (Map Position Player) [(Position, Player)]
	deriving (Eq, Show)

data FinishedBoard =
	FinishedBoard Board GameResult
	deriving (Eq, Show)

data Position =
	Position (Index, Index)
	deriving (Ord, Eq, Show)

data Index =
	LL | MM | RR
	deriving (Ord, Eq, Show)

data NonEmptyBoard =
	Unfinished Board | Finished FinishedBoard 
	deriving (Show)

data NonFinishedBoard =
	NotEmpty Board | Empty
	deriving (Show)

data Outcome =
	InPlay Board | InvalidMove Board | Done FinishedBoard
	deriving (Show)

data GameResult =
	Draw | Winner Player
	deriving (Show, Eq)

move0 :: Player -> Position -> Board
move0 player position = Board (Map.fromList [firstMove]) [firstMove]
	where firstMove = (position, player)

board :: Outcome -> Board 
board (InPlay board) = board
board (Done (FinishedBoard board result)) = board
board (InvalidMove board) = board

move :: Board -> Position -> Outcome
move (Board map moveHistory) position
	| Map.member position map = InvalidMove (Board map moveHistory)
	| result /= Nothing = Done (FinishedBoard newBoard (Maybe.fromJust result))
	| otherwise = InPlay newBoard
    where newBoard = placePlayer (Board map moveHistory) position
          result = finished newBoard

placePlayer :: Board -> Position -> Board
placePlayer (Board map moveHistory) position = Board (Map.insert position player map) ((position, player):moveHistory)
    where player = switch (snd(head moveHistory))

switch :: Player -> Player
switch Naught = Cross
switch Cross = Naught

move' :: Outcome -> Position -> Outcome
move' (InvalidMove board) _ = InvalidMove board
move' (InPlay board) position = move board position 
move' (Done finishedBoard) _ = Done finishedBoard


finished :: Board -> Maybe GameResult
finished (Board map moves)
	| hasWinner (Board map moves) = Just (Winner (snd(head moves)))
	| length moves == 9 = Just Draw
	| otherwise = Nothing


hasWinner :: Board -> Bool
hasWinner board@(Board map moves) = isRowFilled board position player || isColumnFilled board position player || areDiagonalsFilled board player
    where player = snd(head moves)
          position = fst(head moves)

isRowFilled :: Board -> Position -> Player -> Bool
isRowFilled board (Position (row, _)) player = occupiesPlaces board player [Position (row, LL), Position (row, MM), Position (row, RR)]

isColumnFilled :: Board -> Position -> Player -> Bool
isColumnFilled board (Position (_, col)) player = occupiesPlaces board player [Position (LL, col), Position (MM, col), Position (RR, col)]


areDiagonalsFilled :: Board -> Player -> Bool
areDiagonalsFilled board player = occupiesPlaces board player [Position (LL, LL), Position (MM, MM), Position (RR, RR)] ||
	occupiesPlaces board player [Position (LL,RR), Position (MM, MM), Position (RR, LL)]

occupiesPlaces :: Board -> Player -> [Position] -> Bool
occupiesPlaces _ _ [] = True
occupiesPlaces (Board map moves) player (x:xs)
	| Map.lookup x map == Just player = occupiesPlaces (Board map moves) player xs
	| otherwise = False  


playerAt :: Board -> Position -> Maybe Player
playerAt (Board map _) position = Map.lookup position map

whoWon :: FinishedBoard -> GameResult
whoWon (FinishedBoard _ result) = result 

takeBack :: NonEmptyBoard -> NonFinishedBoard
takeBack (Finished (FinishedBoard board _)) = takeBack (Unfinished board)
takeBack (Unfinished (Board _ (previousMove:[]))) = Empty 
takeBack (Unfinished (Board map ((position, player):xs))) = NotEmpty (Board (Map.delete position map) xs)

takeBack' :: NonFinishedBoard -> NonFinishedBoard
takeBack' Empty = Empty
takeBack' (NotEmpty board) = tackBack (NonEmpty board)

prettyPrint :: Board -> [[Char]]
prettyPrint board = [[printCell board (Position (LL, LL)), printCell board (Position (LL, MM)), printCell board (Position (LL, RR))],
					[printCell board (Position (MM, LL)), printCell board (Position (MM, MM)), printCell board (Position (MM, RR))],
					[printCell board (Position (RR, LL)), printCell board (Position (RR, MM)), printCell board (Position (RR, RR))]]

printCell :: Board -> Position -> Char
printCell (Board map _) position
	| Map.lookup position map == Just Naught = 'O'
	| Map.lookup position map == Just Cross = 'X'
	| otherwise = '-'


instance Arbitrary Index where
	arbitrary = elements [LL, MM, RR]

instance Arbitrary Player where
	arbitrary = elements [Naught, Cross]

--instance Arbitrary Board where
--	arbitrary = do 
--		p <- arbitrary
--		ps <- arbitrary
--		return $ foldr propell (start p) ps

--propell :: Position -> Board -> Board
--propell p b =
--	case move b p of 
--		InvalidMove -> b
--		Done _ -> b
--		InPlay b' -> b'





 