import Data.Function
import Data.List

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- length' xs = sum [1 | _ <- xs]
length' :: (Num b) => [a] -> b
-- this is the edge case, and the trick to stop the recursion
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- 'ham'
-- 1 + (length' "am")
-- 1 + (1 + length' "m")
-- 1 + (1 + 1 + length' "")
-- 1 + (1 + 1 + 0)
-- 3

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- we don't have to pass a list because that's anyways a partially applied function, so we are anyawys going to pass a list lol

-- sum' [] = 0
-- sum' (x : xs) = x + sum' xs

-- [1, 2, 3, 4]
-- 1 + sum' [2, 3, 4]
-- 1 + (2 + sum' [3, 4])
-- 1 + (2 + (3 + sum' [4]))
-- 1 + (2 + (3 + (4 + sum' [])))
-- 1 + (2 + (3 + (4 + 0)))
-- 10

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- [1, 2, 3, 4] is syntactic sugar for 1:2:3:4:[]
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : y : z) = x

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : y) = y

-- "Dracula" is syntactic sugar for 'D':'r':'a':'c':'u':'l':'a':[]
-- "Hello" is syntactic sugar for 'H':'e':'l':'l':'o':[]

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- currying demonstration
-- max' 4 5
-- (max' 4) 5
-- 5

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

-- where bindings are shared across the guards

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "can't call maximum on an empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

-- replicate' 3 5
-- 5 : (replicate' 2 5)
-- 5 : (5 : replicate' 1 5)
-- 5 : (5 : 5 : replicate' 0 5)
-- 5 : (5 : 5 : [])
-- [5, 5, 5]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n li
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

--  [3, 5, 1, 2, 4]
--  [1, 2] ++ [3] ++ [5, 4]
--  [1] ++ [2] ++ [3] ++ [5, 4]
--  [1] ++ [2] ++ [3] ++ [4] ++ [5]
--  [1, 2, 3, 4, 5]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (+3) 10
-- (+3) ((+3) 10)
-- (+3) (13)
-- 16

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- the end is going to be an empty list anyways, so use `:` safely

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- or
-- flip' f = \x -> \y -> f y x
-- flip' = \f -> \x -> \y -> f y x
-- flip' = \f x y -> f y x

-- addThree :: (Num a) => a -> a -> a -> a
-- addThree = \x -> \y -> \z -> x + y + z

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldl (\acc x -> f x : acc) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x acc -> f x : acc) []

-- or
-- map' _ [] = []
-- map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | odd x = x : chain (1 + (x * 3))

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs >= 15

addExample :: (Num a) => a -> a -> a
addExample x y = x + y

-- addExample = \x -> \y -> x + y

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> (x == y) || acc) False

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

flipList :: [a] -> [a]
flipList = foldl (flip (:)) []

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl
        (\acc x -> (take nlen x == needle) || acc)
        False
        (tails haystack)

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)

-- (==) `on` (> 0)

-- (==) ((>0) x) ((>0) y)
-- \x y -> (x > 0) == (y > 0)
