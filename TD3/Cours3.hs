module Cours3 where

import Prelude
import Data.Maybe
import Control.Applicative

guard :: Monad m => Bool -> m ()
guard test = if test then return () else fail "bad guard"

greaterThan30 :: (Monad m, Ord a, Num a) => m a -> m a -> m(a, a)
greaterThan30 x y = do
    a <- x
    b <- y
    guard $ a * b > 30
    return (a, b)

--greaterThan30 x y = do
--    a <- x
--    b <- y
--    guard $ a * b > 30
--    return (a, b)

--data (Monoid w) => Writer w a = { runWriter :: (a , w) }

-- Reader contient une fonction de r dans a
data Reader r a = Reader{ runReader :: r -> a }

instance Functor (Reader e) where
    --on peut appliquer une fonction à la valeur
    --fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f (Reader r) = Reader $ \ e -> f (r e)

instance Monad (Reader e) where
    return = Reader . const
    (Reader r) >>= f = Reader $ \e -> runReader (f $ r e) e

--ask :: Reader e e 
--ask = Reader id

--hello :: Reader [(String, String)] String
--hello = do
--    env <- ask 
--    let firstName = fromJust $ (lookup "first" env)
--    return "Hello" ++ firstName

data State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
    fmap f (State rs) = State $ \ s -> 
        let (a, s') = rs s in (f a, s')

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State rs) >>= f = State $ \s -> 
        let (a, s') = rs s
        in let (State rs') = f a
            in rs' s' 

instance (Monoid wt) = > Applicative (State s) where
    pure = return
    (<*>) = ap 

put :: s -> State s ()
put x = State $ \s -> ((), x)

get :: State s s
get = State $ \s -> (s, s)

change :: (s -> s) -> State s ()
change f = do
    current <- get
    put $ f current

--fact :: Int -> State Int Int
--fact 0 = do 
--    put 0
--    return 1
--fact n = do
--    t <- fact (n-1) 
--    z <- get
--    put (z+1)
--    return $ n * t

dummy :: State Int ()
dummy = do
    put 1
    put 2
    return ()
