module Peano where

data Peano = Zero | Succ Peano
    deriving (Show)

addTwo :: Peano -> Peano
addTwo x = Succ (Succ x)

instance Num Peano where
    a + b = case b of 
        Zero -> a
        Succ b' -> (Succ a) + b'

    a * b = case b of 
        Zero -> Zero
        Succ b' -> (a * b') + a

    abs a = a

    signum a = case a of
        Zero -> Zero
        Succ _ -> Succ Zero

    fromInteger a = if a <= 0 then Zero else Succ (fromInteger (a-1))
    
