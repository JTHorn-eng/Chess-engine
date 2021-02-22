
module Board where
import qualified Toolkit as Tool

data Piece = Blank | Pawn | Knight | Queen | King | Bishop | Rook deriving (Show, Eq)
-- row, column, score, type, side (1 - White, -1 - Black, 0 - Neutral), captured (True/False)
data PieceInfo = PieceInfo {row :: Int, col :: Char, sco :: Int, typ :: Piece, sid :: Int, cap :: Boolean} deriving (Show, Eq)
type Board = [PieceInfo]

-- generate board, keep track of board status
init :: Board
init = result where
    result  = pawns ++ rooks ++ queens ++ kings ++ bishops ++ knights ++ blanks
    knights = [PieceInfo {row=r, col=c, sco=3, typ=Knight, sid=(if row == 1 then 1 else negate 1), cap=False} | c <- ['b', 'g'], r <- [1, 8]]
    bishops = [PieceInfo {row=r, col=c, sco=3, typ=Bishop, sid=(if row == 1 then 1 else negate 1), cap=False} | c <- ['c', 'f'], r <- [1, 8]]
    kings   = [PieceInfo {row=(if c == 'd' then 8 else 1), col=c, sco=10000000, typ=King, sid=(if col == 'd' then negate 1 else 1), cap=False} | c <- ['d', 'e']]
    queens  = [PieceInfo {row=(if col == 'd' then 1 else 8), col=c, sco=7, typ=Queen, side=(if col == 'd' then 1 else negate 1), cap=False} | c <- ['d', 'e']]
    rooks   = [PieceInfo {row=r, col=c, sco=5, typ=Rook, sid=(if row == 1 then 1 else negate 1), cap=False} | c <- ['a', 'h'], r <- [1, 8]]
    pawns   = [PieceInfo {row=r, col=c, sco=1, typ=Pawn, sid=(if row == 2 then 1 else negate 1), cap=False} | c <- ['a'..'h'], r <- [2, 7]]
    blanks  = [PieceInfo {row=r, col=c, sco=0, typ=Blank, sid=0, cap=False} | c <- ['a'..'h'], r <- [3..6]]

insertPiece :: PieceInfo -> Board -> Int -> Char -> Board
insertPiece p bo r c = map (\pi-> if row pc == r && col pc == c then np else pi) bs
    where
    np = PieceInfo {row=r, col=c, sco=sco p, typ=typ p, sid=sid p, cap=cap p}
    pc = (search bo r c)

-- board, row, column, outputs piece
search :: Board -> Int -> Char -> PieceInfo
search [] r c = PieceInfo {row=r, col=c, sco=0, typ=Blank, sid='N', cap=False}
search (x:xs) r c = 
    | (r == row x) && (c == col x) = x
    | otherwise = searchBoard xs r c

initSingleton :: [[String]]
initSingleton = take 128 $ repeat '@'

display :: Board -> String -> String
display [] input = show input
display (x:xs) input = display xs (Tool.replaceAtIndex piece input (8 * Tool.charToInt (row x)) + (col x))
    where 
        piece = pieceType ++ [(if (sid x)=='N' then " " else sid x]
        pieceType = 
            | (typ x == Blank) = "-"
            | (typ x == Bishop) = "B"
            | (typ x == Knight) = "H"
            | (typ x == Pawn) = "P"
            | (typ x == Rook) = "R"
            | (typ x == Queen) = "Q"
            | (typ x == King) = "K"


canMove :: PieceInfo -> Board -> Boolean
canMove piece board =
      | (typ x == Blank) = False
      | (typ x == Pawn) = canMovePawn piece board
      | (typ x == Knight) = "H"
      | (typ x == Pawn) = "P"
      | (typ x == Rook) = "R"
      | (typ x == Queen) = "Q"
      | (typ x == King) = "K"

colChng :: PieceInfo -> Int -> Int
colChng piece c = Tool.intToChar ((Tool.charToInt (col piece)) + (sid piece * c))


--for a given piece, find potential states of movement only

move :: PieceInfo -> Board -> [Int] -> [Int] -> States
move piece board rowOps colOps = states
    where
    states = map (\p -> insertPiece p board (row p) (col p)) filteredPieces
    filteredPieces = filter (\p -> typ p == Blank) foundPieces
    foundPieces = map changeApply pieceChange
    changeApply = (\(r,c) -> search board (row piece + r) (colChng piece c))
    pieceChange = zip rowOps colOps


--for a given place, find potential states of capture only
    
capture :: PieceInfo -> Board -> [Int] -> [Int] -> States
capture piece board rowOps colOps = map (\p -> insertPiece p board (row p) (col p)) foundPieces
    where
    foundPieces = map changeApply pieceChange
    changeApply = (\(r,c) -> search board (row piece + r) (colChng piece c))
    pieceChange = zip rowOps colOps




















