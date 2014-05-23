import Prelude
import Data.Ratio
import Data.List
import Data.Maybe (fromMaybe)
-- Prob contient une liste de valeurs associés à des probas
data Prob a = Prob [ (a, Ratio Int) ] deriving Show


applyOnFirst :: (a -> fa) -> ((a, b) -> (fa,  b))
applyOnFirst f = \(a, b) -> (f a, b)

applyOnSecond :: (b -> fb) -> ((a, b) -> (a,  fb))
applyOnSecond f = \(a, b) -> (a, f b)

sameProbability :: [a] -> Prob a
sameProbability = \ l ->  Prob $ map (\ x -> (x, 1 % length l)) l 

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (applyOnFirst f) xs

multiplyProb :: (Prob a, Ratio Int) -> [(a, Ratio Int)]
multiplyProb (Prob l, x) = map (applyOnSecond (*x)) l

concatProb :: Prob (Prob a) -> Prob a
concatProb (Prob l) = Prob $ concat $ map multiplyProb l 

instance Monad Prob where
    return = \a -> Prob [(a, 1 % 1)]
    p >>= f = concatProb (fmap f p)
    fail _ = Prob []

sumSecond :: (Num b) => [(a, b)] -> b
sumSecond [] = 0
sumSecond ((_, b):l) = b + (sumSecond l)

simplify :: (Eq a) => Prob a -> a -> ((a, Ratio Int), [(a, Ratio Int)])
simplify (Prob l) a = 
    let (la, lb) = partition (\ (x, _) -> x == a) l in
        ((a, sumSecond la) , lb)

add :: (a, Ratio Int) -> Prob a -> Prob a
add ap (Prob l) = Prob (ap:l)

canonize :: (Eq a) => Prob a -> Prob a
canonize (Prob []) = Prob []
canonize l@(Prob ((a,_):ls)) = 
    let (ap, lb) = simplify l a in
        add ap (canonize (Prob lb))

renormalize :: Prob a -> Prob a
renormalize pl@(Prob l) = Prob $ multiplyProb (pl, (1%1) / (sumSecond l) )

probability :: (Eq a) => a -> Prob a -> Ratio Int
probability a p = 
    let Prob l = canonize p in
        fromMaybe (0%1) $ lookup a l

reply :: String -> Prob String
reply "Good Morning" = Prob [
    ("You too, sir", 8%10),
    ("Leave me alone", 2%10)]
reply "Fuck off" = Prob [
    ("Leave me alone", 8%10), 
    ("***Punch in the face***", 2%10)]

hello = Prob [("Good Morning", 8%10), ("Fuck off", 2%10)]

dice = sameProbability [1..6]

double :: Prob Bool
double = do
  x <- dice
  y <- dice
  return $ x == y

pair :: Prob Int
pair = do
  x <- dice
  y <- dice
  return $ x + y

sick = Prob [(True, 1%100000), (False, 99999%100000)]
positive :: Bool -> Prob Bool
positive True = Prob [(True, 999%1000), (False, 1%1000)]
positive False = Prob[(False, 999%1000), (True, 1%1000)]

removeNegative :: Bool -> Prob Bool
removeNegative b | b == False = fail "test negatif" 
                 | b == True  = sameProbability [True]

results = renormalize $ sick >>= positive >>= removeNegative

cleave :: a -> [a->b] -> [b]
cleave a [] = []
cleave a (f:l) = (f a):(cleave a l)

spread :: [a->b] -> [a] -> [b] 
spread (f:lf) (a:la) = (f a):(spread lf la)
spread [] [] = []