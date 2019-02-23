--Marek Padlewski

import Data.Char(digitToInt)
import System.Random

getDigit :: IO Char
getDigit = do x <- getChar
              if elem x ['0','1','2','3','4','5','6','7','8','9'] then
                return x
              else
                getDigit


getRandDigit :: Int -> IO Int
getRandDigit range = do x <- randomRIO (1,range)
                        return x

getRandBoard :: Int -> Int -> [IO Int]
getRandBoard _ 0 = []
getRandBoard range amount =
  (getRandDigit range) : (getRandBoard range (amount-1))


test = getRandBoard 5 5
t1 = getRandDigit 5
t2 = head test


              


showBoard :: [Int] -> Int -> Int -> IO ()
showBoard [] _ _ = putStrLn ""
showBoard (x:xs) n i =
  if i < (n+1) then
    do
      putStr ((show i) ++ ": ")
      boardLine x
      showBoard xs n (i+1)
  else
    putStrLn ""
  where
    boardLine :: Int -> IO ()
    boardLine x =
      if x > 0 then
        do
          putStr "*"
          boardLine (x-1)
      else
        putStrLn ""


gameplay :: [Int] -> IO ()
--TODO zamiast argumentu xs zrobic losowanie listy
gameplay xs  =
  do putStr "Podaj ilosc wierszy (1-9): "
     n <- getChar
     gameLoop xs
     where
       gameLoop :: [Int] -> IO ()
       gameLoop xs =
         do
           showBoard xs (length xs) 1
           putStrLn "Podaj numer wiersza i ilosc kamykow: "
           r <- getDigit
           a <- getDigit
           gameLoop (delFromBoard (digitToInt r) (digitToInt a) xs 1)
           where
             delFromBoard :: Int -> Int -> [Int] -> Int -> [Int]
             delFromBoard _ _ [] _ = []
             delFromBoard r a (x:xs) i =
               if i == r then
                 (x-a):xs
               else
                 x:delFromBoard r a xs (i+1)
                 
               

play = gameplay [5,3,1,4,2]



      
