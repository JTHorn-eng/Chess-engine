import qualified Toolkit as Tool

module Board where


data Piece = Blank | Pawn | Knight | Queen | King | Bishop | Rook deriving (Show, Eq)
-- row, column, score, type, side ('W' - White, 'B' - Black, 'N' - Neutral), captured (True/False)
data PieceInfo = PieceInfo {row :: Int, col :: Char, sco :: Int, typ :: Piece, sid :: Char, cap :: Boolean} deriving (Show, Eq)
type Board = [PieceInfo]




-- generate board, keep track of board status
init :: Board
init = result where
    result  = pawns ++ rooks ++ queens ++ kings ++ bishops ++ knights ++ blanks
    knights = [PieceInfo {row=r, col=c, sco=3, typ=Knight, sid=(if row == 1 then 'W' else 'B'), cap=False} | c <- ['b', 'g'], r <- [1, 8]]
    bishops = [PieceInfo {row=r, col=c, sco=3, typ=Bishop, sid=(if row == 1 then 'W' else 'B'), cap=False} | c <- ['c', 'f'], r <- [1, 8]]
    kings   = [PieceInfo {row=(if c == 'd' then 8 else 1), col=c, sco=10000000, typ=King, sid=(if col == 'd' then 'B' else 'W'), cap=False} | c <- ['d', 'e']]
    queens  = [PieceInfo {row=(if col == 'd' then 1 else 8), col=c, sco=7, typ=Queen, side=(if col == 'd' then 'W' else 'B'), cap=False} | c <- ['d', 'e']]
    rooks   = [PieceInfo {row=r, col=c, sco=5, typ=Rook, sid=(if row == 1 then 'W' else 'B'), cap=False} | c <- ['a', 'h'], r <- [1, 8]]
    pawns   = [PieceInfo {row=r, col=c, sco=1, typ=Pawn, sid=(if row == 2 then 'W' else 'B'), cap=False} | c <- ['a'..'h'], r <- [2, 7]]
    blanks  = [PieceInfo {row=r, col=c, sco=0, typ=Blank, sid='N', cap=False} | c <- ['a'..'h'], r <- [3..6]]

search :: Board -> Int -> Char -> PieceInfo
search [] r c = PieceInfo {row=r, col=c, sco=0, typ=Blank, sid='N', cap=False}
search (x:xs) r c = 
    | (r == row x) && (c == col x) = x
    | otherwise = searchBoard xs r c

initSingleton :: [[String]]
initSingleton = take 128 $ repeat '@'

display :: Board -> String -> String
display [] input = show input
display (x:xs) input = display xs (input !! (8 * Tool.charToInt (row x)) + (col x))
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
   















