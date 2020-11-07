import qualified Sorting as Sort
import Graph
import Control.Monad

main = do
    putStr "Setup"
    c <- getChar
    when (c /= ' ') $ do 
        putChar c 
        main