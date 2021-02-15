module Board where


data Piece = Pawn | Knight | Queen | King | Bishop | Rook
-- row, column, score, type, side (True - White, False - Black)
type PieceInfo = (Int, Char, Int, Piece, Boolean)
type Board = [PieceInfo]

-- generate board, keep track of board status
initBoard :: Board
initBoard = result where
    result = pawns ++ rooks ++ queens ++ kings ++ bishops ++ knights
    knights =[(row, col, 3, Knight, (if row == 1 then True else False)) | col <- ['b', 'g'], row <- [1, 8]]
    bishops = [(row, col, 3, Bishop, (if row == 1 then True else False)) | col <- ['c', 'f'], row <- [1, 8]]
    kings = [((if col == 'd' then 8 else 1), col, 10000000, King, (if col == 'd' then False else True)) | col <- ['d', 'e']]
    queens = [((if col == 'd' then 1 else 8), col, 7, Queen, (if col == 'd' then True else False)) | col <- ['d', 'e']]
    rooks = [(row, col, 5, Rook, (if row == 1 then True else False)) | col <- ['a', 'h'], row <- [1, 8]]
    pawns = [(row, col, 1, Pawn, (if row == 2 then True else False)) | col <- ['a' .. 'h'], row <- [2, 7]]


updateBoard :: Board
updateBoard = []








