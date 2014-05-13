module Cours1 where
-- create a git repository
-- Github samueltardieu

-- Types de bases: Int, Integer, Char, Bool, Fractional, Unit

import Prelude hiding (length, flip, (.),
    Maybe, Nothing, Just, fmap,
    Functor, ($))

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

data Maybe a = Just a | Nothing
    deriving (Show)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Functor [] where
    fmap = map

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
infix 0 <$>

($) :: (a -> b) -> a -> b
f $ x = f x
infix 0 $

data Tree a = Empty | Branch (Tree a) a (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Branch l a r) = Branch (f <$> l) (f a) (f <$> r)

class Applicative a where
    pure :: x -> a x
    (<*>) :: a (x -> y) -> a x -> a y
    infix 0 <*>

instance Applicative Maybe where
    pure = Just
    _ <*> Nothing = Nothing
    Nothing <*> _ = Nothing
    (Just f) <*> (Just x) = Just $ f x

plus :: Int -> Int -> Int
plus = (+)

