module RPN where

import System.IO

parseOp :: (Integral a, Read a) => String -> [a] -> [a]
parseOp instruction = 
    case instruction of 
        "+"     ->  \(x : y : s)  ->  (x + y) : s
        "-"     ->  \(x : y : s)  ->  (x - y) : s
        "*"     ->  \(x : y : s)  ->  (x * y) : s
        "/"     ->  \(x : y : s)  ->  (div x y) : s
        "dup"   ->  \(x : s)  ->  x : x : s
        "swap"  ->  \(x : y : s)  ->  y : x : s
        "drop"  ->  \(x : s)  ->  s
        "pick"  ->  \(n : s)  ->  (pick n (n : s)) : s
        "depth" ->  \s  ->  (length' s) : s
        "quit"  -> error "quitting"
        "flush" -> \ _ -> []
        int     ->  \s -> (read int) : s

pick :: (Integral a) => a -> [a] -> a
pick 0 (x : _) = x
pick n (_ : s) = pick (n-1) s

length' :: (Integral a) => [a] -> a
length' [] = 0
length' (_ : s) = (fromInteger 1) + length' s

eval :: a -> [a -> a] -> a
eval x [] = x
eval x (f : fs) = eval (f x) fs

parse :: (Integral a, Read a) => String -> [ [a] -> [a] ]
parse instruction = map parseOp (words instruction)

repl :: (Integral a, Read a, Show a) => [a] -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack

main = repl []

