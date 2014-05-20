module Cours2 where

import Control.Monad --Functor, Monad, MonadPlus
import Control.Applicative
import Data.Monoid

facts :: [Integer]
facts = 1 : zipWith (*) facts [1..]

fact = (facts !!)

fathers = [("Daniel", "Jacques"), ("Jacques", "Toto"), 
    ("Toto", "Oldelaf"), ("Oldelaf", "Bison Futé"),
    ("Giedre", "Jean")]

--lookup x l cherche l'element x dans l
--flip lookup l x cherche l'element x dans l
father :: String -> Maybe String
father = flip lookup fathers

grandfather x = father x >>= father
grandgrandfather x = father x >>= father >>= father

--MonadPlus are Monad with zero or more possible values
--Nothing mplus xs = xs
--Just x mplus _ = Just x
advlookup :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
advlookup a ((k, v) : xs) | a == k    =  (return v) `mplus` advlookup a xs
                          | otherwise = advlookup a xs
advlookup a [] = fail []

data Writer wt at = Writer {runWriter :: (at, wt)} 
    deriving (Show)

instance Functor (Writer wt) where
    fmap f (Writer (a, w)) = Writer(f a, w)  

instance (Monoid wt) => Applicative (Writer wt) where
    pure = return
    (<*>) = ap

instance (Monoid wt) => Monad (Writer wt) where
    return x = Writer $ (x, mempty)
    Writer (a, w) >>= f = let Writer (a', w') = f a
                          in Writer (a', w `mappend` w')

tell :: w -> Writer w ()
tell x = Writer ((), x)

fact' :: Int -> Writer [String] Int
fact' 0 = do 
    tell ["Computing fact 0"]
    return 1
fact' n = do
    t <- fact' (n-1) 
    tell $ ["Computing fact n == " ++ show n ]
    return $ n * t

compute :: Writer w a -> a
    