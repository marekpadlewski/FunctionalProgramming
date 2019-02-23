--Marek Padlewski

import Data.Char(digitToInt)
import System.Random
import System.IO
import Control.Monad

getDigit :: IO Char
getDigit = do x <- getChar
              if elem x ['0','1','2','3','4','5','6','7','8','9'] then
                return x
              else
                getDigit


showBoard :: [Int] -> Int  -> IO ()
showBoard [] _ = putStrLn ""
showBoard (x:xs) n =
  do
    putStr ((show n) ++ ": ")
    boardLine x
    showBoard xs (n+1)
      where
        boardLine :: Int -> IO ()
        boardLine 0 = putStrLn ""
        boardLine n =
          do
            putStr " * "
            boardLine (n-1)
       

changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1


checkEnd :: [Int] -> Bool
checkEnd xs =
  all (\x -> x == 0) xs
  

finalMessage :: Int -> IO ()
finalMessage p =
  if p == 1 then
    putStrLn "Brawo, wygrałeś!"
  else
    putStrLn "Wygrał komputer!" 


gameplay :: IO ()
gameplay  =
  do putStr "Podaj ilosc wierszy (1-9): "
     n <- getLine
     g <- newStdGen
     let size = (read n :: Int)
     let board = (take size (randomRs (1, size) g :: [Int]))
     let player = (head (take 1 (randomRs (1, 2) g :: [Int])))
     showBoard board 1
     gameLoop board player
     where
       gameLoop :: [Int] -> Int -> IO ()
       gameLoop xs p =
         if checkEnd xs then
           finalMessage (changePlayer p)
         else
           if p == 1 then
             do
               putStr "Podaj numer wiersza i ilosc kamyków: "
               rc <- getDigit
               ac <- getDigit
               let r = digitToInt rc
               let a = digitToInt ac
               putStrLn ((show r) ++ " " ++ (show a))
               --sprawdzam czy wprowadzone dane przez gracza sa poprawne
               if r <= (length xs) && xs!!(r-1) >= a && a /= 0 then
                 do
                   let boardAfterMove = delFromBoard r a xs 1
                   showBoard boardAfterMove 1
                   gameLoop boardAfterMove (changePlayer p)
               else
                   gameLoop xs p
                 
           else
             do
               g <- newStdGen
               let size = (length xs)
               let l = (take 2 (randomRs (1, size) g :: [Int]))
               let r = (head l)
               let a = (last l)
               let elem = xs!!(r-1)
               if elem >= a  then
                 do
                   putStrLn ("Ruch komputera: " ++ (show r) ++ " " ++ (show a) ++ "\n")
                   let boardAfterMove = delFromBoard r a xs 1
                   showBoard boardAfterMove 1
                   gameLoop boardAfterMove (changePlayer p)
               else
                 gameLoop xs p  
            where
             delFromBoard :: Int -> Int -> [Int] -> Int -> [Int]
             delFromBoard _ _ [] _ = []
             delFromBoard r a (x:xs) i =
               if i == r then
                 if (x-a) < 0 then
                    0:xs
                 else
                    (x-a):xs
               else
                 x:delFromBoard r a xs (i+1)
                 
               

nimGame = gameplay



      
