module Cours1 where
-- create a git repository
-- Github samueltardieu

-- Types de bases: Int, Integer, Char, Bool, Fractional, Unit

import Prelude hiding (length, flip, (.) )

increment :: Int -> Int
increment x = x + 1

length :: [a] -> Int
length l = if null l
           then 0
           else 1 + length (tail l)

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

length'' :: [a] -> Int
length'' l = case l of 
    []  -> 0
    (_ : xs) -> 1 + length'' xs

first :: a -> b -> a
first x _ = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

second :: a -> b -> b
second = flip first

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

length_plus_one = increment.length

