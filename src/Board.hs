module Board where

import qualified Toolkit as Tool
import qualified Data.Char as Char
import qualified Debug.Trace as Trace

data Piece = Empty | Blank | Pawn | Knight | Queen | King | Bishop | Rook deriving (Show, Eq)
-- row, column, score, type, side (1 - White, -1 - Black, 0 - Neutral)
data PieceInfo = PieceInfo {row :: Int, col :: Char, sco :: Int, typ :: Piece, sid :: Int} deriving (Show, Eq)

type Game = ([PieceInfo], [PieceInfo]) -- (Board, captured pieces)
type Board = [PieceInfo]
type States = [Game]
type Coords = [(Int, Int)]

--DEBUGGING
--PieceInfo {row=1, col='c', sco=3, typ=Knight, sid=1} white knight piece
--PieceInfo {row=1, col='e', sco=7, typ=Queen, sid=1} white queen piece
--PieceInfo {row=1, col='d', sco=10000000, typ=King, sid=1} white king piece
--PieceInfo {row=0, col='a', sco=0, typ=Empty, sid=0}  empty piece
--PieceInfo {row=4, col='b', sco=0, typ=Blank, sid=0}  blank piece
--PieceInfo {row=7, col='c', sco=1, typ=Pawn, sid=negate 1}  black pawn piece
--PieceInfo {row=2, col='c', sco=1, typ=Pawn, sid=1}  white pawn piece
--PieceInfo {row = 3, col = 'g', sco = 0, typ = Blank, sid = 0} a blank square (isPath detection)

-- generate board, keep track of board status
initBoard :: Board
initBoard = result where
    result  = pawns ++ rooks ++ queens ++ kings ++ bishops ++ knights ++ blanks
    knights = [PieceInfo {row=r, col=c, sco=3, typ=Knight, sid=(if r == 1 then 1 else negate 1)} | c <- ['b', 'g'], r <- [1, 8]]
    bishops = [PieceInfo {row=r, col=c, sco=3, typ=Bishop, sid=(if r == 1 then 1 else negate 1)} | c <- ['c', 'f'], r <- [1, 8]]
    kings   = [PieceInfo {row=(if c=='d' then 8 else 1), col=c, sco=10000000, typ=King, sid=(if c=='d' then (negate 1) else 1)} | c <- ['d', 'e']]
    queens  = [PieceInfo {row=(if c=='d' then 1 else 8), col=c, sco=7, typ=Queen, sid=(if c== 'd' then 1 else negate 1)} | c <- ['d', 'e']]
    rooks   = [PieceInfo {row=r, col=c, sco=5, typ=Rook, sid=(if r== 1 then 1 else negate 1)} | c <- ['a', 'h'], r <- [1, 8]]
    pawns   = [PieceInfo {row=r, col=c, sco=1, typ=Pawn, sid=(if r== 2 then 1 else negate 1)} | c <- ['a'..'h'], r <- [2, 7]]
    blanks  = [PieceInfo {row=r, col=c, sco=0, typ=Blank, sid=0} | c <- ['a'..'h'], r <- [3..6]]

initGame :: Game
initGame = (initBoard, [])
 
boardSingleton :: String
boardSingleton = take 128 $ repeat '@'
cr_limit = [0,17..128]

displayBoard :: String -> [Int] -> String
displayBoard newString [] = newString
displayBoard newString (x:xs) = displayBoard (Tool.insertAtIndex "\n" (newString) x) xs

genBoardString :: Board -> String -> String
genBoardString [] board = board
genBoardString (x:xs) board =
    genBoardString xs newBoard
    where 
        newBoard = Tool.replaceAtIndex ((getPieceString x) ++ sideToString (sid x)) board index
        index = 2 * (((Tool.charToInt (col x)) ) + (row x - 1) * 8)

displayStates :: States -> IO ()
displayStates [] = putStrLn ""
displayStates (game:games) = 
    do
        putStrLn (displayBoard (genBoardString (fst game) boardSingleton) cr_limit) 
        displayStates games

getPieceString :: PieceInfo -> String
getPieceString piece 
    | (typeP == Blank) =  "_"
    | (typeP == Pawn) =   "P"
    | (typeP == Knight) = "N"
    | (typeP == Rook) =   "R"
    | (typeP == Bishop) = "B"
    | (typeP == King) =   "K"
    | (typeP == Queen) =  "Q"
    where
        typeP = typ piece

sideToString :: Int -> String
sideToString x = if x == 1 then "W" else (if x == negate 1 then "B" else "_")

stringToSide :: Char -> Int
stringToSide x = if x == 'W' then 1 else (if x == 'B' then negate 1 else 0)

possibleMoves :: Piece -> Int -> Coords
possibleMoves piece side
    | ( piece == Blank) = error "Cannot move a blank"
    | ( piece == Pawn) = [(1 * side,0), (1 * side, 1), (1 * side, negate 1)]
    | ( piece == Knight) = [(x,y) | x <- [negate 1, 1], y <-[negate 2, 2]] ++ [(x,y) | x <- [negate 2, 2], y <-[negate 1, 1]]
    | ( piece == Bishop) = Tool.removeFromList ([(x,x) | x<-[negate 7..7]] ++ [(x,negate x) | x<-[negate 7..7]] ++ [(negate x, x) | x<-[negate 7..7]]) (0,0)
    | ( piece == Rook) = Tool.removeFromList ([(x,0)|x<-[negate 7..7]] ++ [(0,y)|y<-[negate 7..7]]) (0,0)
    | ( piece == Queen) = Tool.removeFromList ([(x,negate x) | x<-[negate 7..7]] ++ [(negate x, x) | x<-[negate 7..7]] ++ [(x,x) | x<-[negate 7..7]] ++ [(x,0)|x<-[negate 7..7]] ++ [(0,y)|y<-[negate 7..7]]) (0,0)
    | ( piece == King) = (Tool.removeFromList [(x,y) | x<-[negate 1..1], y<-[negate 1..1]] (0,0)) 

pieceToType :: String -> Piece
pieceToType piece 
    | (last piece == 'K') = King
    | (last piece == 'Q') = Queen
    | (last piece == 'B') = Bishop
    | (last piece == 'N') = Knight
    | (last piece == 'R') = Rook
    | (last piece == 'P') = Pawn 

findPieceInBoard :: String -> Board -> PieceInfo
findPieceInBoard piece board = 
 (filter (\x -> findPieceInBoardHelper piece x) board) !! 0
    
findPieceInBoardHelper :: String -> PieceInfo -> Bool
findPieceInBoardHelper piece p =
    side && pieceType
    where
        side = stringToSide (piece !! 0) == sid p
        pieceType = pieceToType piece == typ p

getPieceAtCoords :: Int -> Int -> Board -> PieceInfo
getPieceAtCoords x y board
    | (x > 0 && x <= 8 && y >= 0 && y < 8 ) = (filter (\p -> ((row p) == x) && (Tool.charToInt (col p)) == y) board) !! 0
    | otherwise = PieceInfo{row=0, col='a',sco=0,typ=Empty,sid=0}

isValidMove :: PieceInfo -> Board -> PieceInfo -> Bool
isValidMove piece board target
    | (typ target == Empty) = False
    | (typ piece == Pawn) = checkPwnPath piece board target
    | (typ piece == Knight) = sid piece /= sid target
    | otherwise = isPathToPiece piece board target

checkPwnPath :: PieceInfo -> Board -> PieceInfo -> Bool
checkPwnPath piece board target
    | (chkPwnDisLeft) = sid target == negate (sid piece)
    | (chkPwnDisRight) = sid target == negate (sid piece)
    | otherwise = isPathToPiece piece board target
    where
        chkPwnDisLeft = ((Tool.charToInt (col target)) - (Tool.charToInt (col piece))) == negate 1
        chkPwnDisRight = ((Tool.charToInt (col target)) - (Tool.charToInt (col piece))) == 1

isPathToPiece :: PieceInfo -> Board -> PieceInfo -> Bool
isPathToPiece source board target = checkSpaces source disX disY board target
    where
        disY = (Tool.charToInt $ col target) - (Tool.charToInt $ col source)
        disX = (row target) - (row source)

checkSpaces :: PieceInfo -> Int -> Int -> Board -> PieceInfo -> Bool
checkSpaces source disX disY board target
    | ((rowCheck && colCheck) && terminationCheck) = True
    | otherwise = pieceAtCheck && (checkSpaces pieceAt (disX - (abs dX)) (disY - (abs dY)) board target)
    where
        terminationCheck =  (sid source == negate (sid target)) || typ target == Blank
        rowCheck = (row source) + dX == row target
        colCheck = (Tool.charToInt (col source)) + dY == Tool.charToInt (col target)
        pieceAtCheck = typ pieceAt == Blank
        pieceAt  = getPieceAtCoords ((row source) + dX) (Tool.charToInt (col source) + dY) board
        dX = if (disX == 0) then 0 else (if (disX < 0) then negate 1 else 1)
        dY = if (disY == 0) then 0 else (if (disY < 0) then negate 1 else 1)

checkSide :: PieceInfo -> PieceInfo -> Bool
checkSide source target = if (sid source == sid target) then False else True    

findPossibleMove :: PieceInfo -> Game -> Board
findPossibleMove piece game =  augmentPieces
    where
        augmentPieces = (\p -> PieceInfo{row=row p, col = col p, sco = sco piece, typ = typ piece, sid = sid piece}) <$> validMoves
        validMoves = findValidMoves game piece

getPiecesByType :: Int -> Board -> Board
getPiecesByType side node = filter (\piece -> sid piece == side ) node

--takes in potential moves and the board outputs the possible child states
changeInState :: Game -> Board -> Board -> States
changeInState _ [] _ = []
changeInState game (x:xs) oldBoard
    | (optl == nptl) = [(newBoard, snd game)] ++ (changeInState game xs oldBoard)
    | (optl /= nptl) = [(newBoard, (snd game) ++ [findCapturedPieces newBoard oldBoard])] ++ (changeInState game xs oldBoard)
    where 
    optl = (length $ getPiecesByType 1 oldBoard) - (length $ getPiecesByType (negate 1) oldBoard) 
    nptl = (length $ getPiecesByType 1 newBoard) - (length $ getPiecesByType (negate 1) newBoard)
    newBoard = (removeFromBoard oldBoard (row x) (Tool.charToInt (col x))) ++ [x]

findCapturedPieces :: Board -> Board -> PieceInfo
findCapturedPieces newB (x:xs)
    | ((x `elem` newB) == True) = findCapturedPieces newB xs
    | ((x `elem` newB) == False) = x

removeFromBoard :: Board -> Int -> Int -> Board
removeFromBoard [] _ _ = []
removeFromBoard (p:ps) x y
    | ((row p == x) && (Tool.charToInt (col p) == y)) = [] ++ removeFromBoard ps x y 
    | otherwise = [p] ++ removeFromBoard ps x y 

-- takes in a side and outputs all states
findAllStates :: Int -> Game -> Board -> States
findAllStates _ _ []  = []
findAllStates side game (x:xs)
   | (side == 1) = newBoard ++ (findAllStates side game xs)
   | (side == negate 1) = newBoard ++ (findAllStates side game xs)
   | otherwise = []
   where
   newBoard = changeInState game (findPossibleMove x game) removedOldPiece
   removedOldPiece = (filter (\piece -> x /= piece) (fst game)) ++ [newBlank]
   newBlank = PieceInfo{row=row x, col=col x, sco=0, typ = Blank, sid =0}

-- takes pieces from a side and the side number
findStatesScores :: Game -> Board -> Int -> [(Board, Int)]
findStatesScores game selectedPieces side = zip (allStatesBoards) scoreMapping
    where
    allStatesBoards = map (\state -> fst state) allStates
    scoreMapping = map (\state -> (nop (fst state) side)) allStates
    allStates = findAllStates side game selectedPieces

maximumState :: Ord a => [(t, a)] -> (t, a)
maximumState []      = error "maximum of empty list"
maximumState (x:xs)  = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

minimumState :: Ord a => [(t, a)] -> (t, a)
minimumState []     = error "minimum of empty list"
minimumState (x:xs) = minTail x xs
  where
  minTail currentMin [] currentMin
  minTail (m, n) (p:ps)
      | n > (snd p) = minTail p ps
      | otherwise   = minTail (m, n) ps
          
--takes in side and returns score for best state
findHeuristic :: Game -> Int -> Int
findHeuristic game side
    | (side == 1) = snd maxWhite
    | (side == negate 1) = snd maxBlack
    | otherwise = error "Invalid state"
    where
        maxWhite = maximumState ssTupleWhite
        maxBlack = minimumState ssTupleBlack
        ssTupleWhite = findStatesScores game (getPiecesByType 1 (fst game)) 1
        ssTupleBlack = findStatesScores game (getPiecesByType (negate 1) (fst game)) (negate 1)

--find all the spaces for a side that a piece can move to
moveableSpaces :: Int -> Game -> Board
moveableSpaces side game = possiblesForType
    where
    possiblesForType = foldl (++) [] (map (\p -> findValidMoves game p) pieces)
    pieces = getPiecesByType side (fst game)

findValidMoves :: Game -> PieceInfo -> Board
findValidMoves game piece = validMoves
    where
    validMoves = filter (\x -> (isValidMove piece (fst game) x)) piecesAtCoords
    piecesAtCoords = [getPieceAtCoords (row piece + fst m) (snd m + (Tool.charToInt $ col piece)) (fst game) | m <-movesList]
    movesList
        | (typ piece == Pawn && (row piece == 2 || row piece == 7)) = (((possibleMoves (sid piece)) . typ) piece) ++ [(2, 0)]
        | otherwise = ((possibleMoves (sid piece)) . typ) piece

isCheck :: Game -> Int -> Bool
isCheck game side = if (king `elem` moves) then True else False
    where 
        moves = moveableSpaces (negate side) game
        king = (filter (\p -> (sid p == side) && (typ p == King)) (fst game)) !! 0

checkmate :: Game -> (Bool, Int)
checkmate game
    | ((bKingMoves == []) && (isCheck game 1)) = (True, negate 1)
    | ((wKingMoves == []) && (isCheck game (negate 1))) = (True, 1)
    | otherwise = (False, 0)
    where
        bKingMoves = Tool.invCompareSets (findValidMoves game bKing) (moveableSpaces 1 game)
        wKingMoves = Tool.invCompareSets (findValidMoves game wKing) (moveableSpaces (negate 1) game)
        wKing = (filter (\p -> (sid p == 1) && (typ p == King)) (fst game)) !! 0
        bKing = (filter (\p -> (sid p == negate 1 ) && (typ p == King)) (fst game)) !! 0

--HEURISTICS FOR STATES
nop :: Board -> Int ->  Int
nop current_board side 
    | (side == 1) = (whites - blacks)
    | (side == negate 1) = (blacks - whites)
    | otherwise = 0
    where 
    blacks = length $ filter (\x -> sid x == negate 1) current_board
    whites = length $ filter (\x -> sid x == 1) current_board

