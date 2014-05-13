module RPN where

import System.IO

type Stack = [Int]

type Operator = Stack -> Stack

parseOp :: String -> Operator
parseOp instruction = 
    case instruction of 
        "+"     ->  \(x : y : s)  ->  (x + y) : s
        "-"     ->  \(x : y : s)  ->  (x - y) : s
        "*"     ->  \(x : y : s)  ->  (x * y) : s
        "/"     ->  \(x : y : s)  ->  (div x y) : s
        "dup"   ->  \(x : s)  ->  x : x : s
        "swap"  ->  \(x : y : s)  ->  y : x : s
        "drop"  ->  \(x : s)  ->  s
        "pick"  ->  \(n : s)  ->  ((n : s) !! n) : s
        "depth" ->  \s  ->  (length s) : s
        "quit"  -> error "quitting"
        "flush" -> \ _ -> []
        int     ->  \s -> (read int :: Int) : s

pick :: Int -> [a] -> a
pick 0 (x : _) = x
pick n (_ : s) = pick (n-1) s

eval :: a -> [a -> a] -> a
eval x [] = x
eval x (f : fs) = eval (f x) fs

parse :: String -> [Operator]
parse instruction = map parseOp (words instruction)

repl :: Stack -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack

main = repl []

