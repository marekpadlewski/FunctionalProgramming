--Marek Padlewski

--Zadanie 1

lrepeat :: (Int -> Int) -> [a] -> [a]
lrepeat f xs = aux xs 0 (f 0)
  where
  aux :: [a] -> Int -> Int -> [a]
  aux (y:ys) i 0 = aux ys (i+1) (f (i+1))
  aux (yl @ (y:ys)) i r = y : aux yl i (r-1)
  aux [] _ _ = []

seq1 :: Int -> [Int]
seq1 x = x:seq1 (x+1)

t11 = lrepeat (\x -> 2*x) [1,2,3]
t12 = lrepeat (\x -> (x+1)*3) []
t13 = take 20 (lrepeat (\x -> x+1) (seq1 2))

--Zadanie 2

sublist :: [Int] -> [a] -> [a]
sublist xs ll = sub 0 ll
  where
    sub :: Int -> [a] -> [a]
    sub _ [] = []
    sub i (h:t) =
      if elem i xs then
        sub (i+1) t
      else
        h : sub (i+1) t

t21 = sublist [0,3,7] [1,2,3,4,5,6,7,8,9]
t22 = sublist [0,1] []
t23 = take 8 (sublist [1,4,7,2] (seq1 10))

--Zadanie 3a

epsilon = 10^^(-15)

root3 :: Double -> Double
root3 a = rt3 (if a > 1 then a/3 else a)
  where
    rt3 :: Double -> Double
    rt3 x = 
      if abs (x**3.0 - a) <= epsilon * abs(a) then
        x
      else
        rt3 (x + (a / (x**2.0) - x) / 3.0)


t31 = root3 (-8.0)
t32 = root3 20.0
t33 = root3 1.0
t34 = root3 0.0
t35 = root3 (1/27)


--Zadanie 3b

root3l :: Double -> Double
root3l a = do
  let x0 = (if a > 1 then a/3 else a)
  let l = iterate (\x -> (x + (a / (x**2.0) - x) / 3.0)) x0
  rt3l l
  where
    rt3l :: [Double] -> Double
    rt3l (hd:tl) =
      if abs(hd**3.0 - a) <= epsilon * abs(a) then
        hd
      else
        rt3l tl


t41 = root3l (-8.0)
t42 = root3l 20.0
t43 = root3l 1.0
t44 = root3l 0.0
t45 = root3l (1/27)


    

