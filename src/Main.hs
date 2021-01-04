module Main where

import Graphics.Gloss

window :: Display
background :: Color
wWidth :: Int
hHeight :: Int

wWidth = 640
hHeight = 640
window = InWindow "Chess" (wWidth, hHeight) (10, 10)
background = white

generateBoard ::  [Picture] -> Int -> Int -> [Picture]
generateBoard colors x y
    | (length colors) < 64 = [boardSquare] ++ generateBoard ([boardSquare] ++ colors) newX newY
    | otherwise = []
    where boardSquare = translate (-centerX + fromIntegral newX) (centerY - fromIntegral newY) $ square
          square = color newC $ rectangleSolid 80 80
          newC = if ((newX + newY) `mod` 160 == 0) then green else white
          newX = (if x == 560 || (length colors == 0) then 0 else x + 80) 
          newY = (if x == 560 then y + 80 else y)
          centerX = fromIntegral ((wWidth - 80) `div` 2)
          centerY = fromIntegral ((hHeight - 80) `div` 2)

generateText :: [Picture]
generateText = map (\lab -> scale 0.2 0.2 $ translate (fromIntegral (fst (fst lab))) (fromIntegral (snd (fst lab))) $ text $ (snd lab):[]) labels
    where
        labels = yLabels ++ xLabels
        yLabels = zip [(-1300 + x, -1570) | x <- map (*5) [0,80..560]] ['A'..'H']
        xLabels = zip [(-1590, -1350 + y) | y <- map (*5) [0,80..560]] ['1'..'8']



drawing :: Picture
drawing = pictures $ (generateBoard [] 0 0) ++ generateText

main :: IO ()
main = display window background drawing
    
