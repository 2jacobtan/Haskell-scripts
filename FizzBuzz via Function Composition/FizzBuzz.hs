-- from https://www.youtube.com/watch?v=FyCYva9DhsI&t=21m03s
-- NDC conference talk: Clean Coders Hate What Happens to Your Code When You Use These Enterprise Programming Tricks

import Data.List (foldl')
import Data.List (intersperse)
import Data.Function ((&))

fizzbuzz :: Integer -> String
fizzbuzz n = fizz (buzz id) $ show n
  where
    --test :: Integer -> String -> (String -> String) -> (String -> String)
    test d s x = case n `mod` d of
      0 -> \_ -> s ++ x ""
      _ -> x
    fizz x = test 3 "Fizz" x
    buzz x = test 5 "Buzz" x

-- Appending on singly linked list works well when first list is short.
outputString = foldr (\n accum -> fizzbuzz n ++ "\n" ++ accum) "" [0..99] 
main = putStr outputString

outputString' = [0..99] & map fizzbuzz & intersperse "\n" & mapM_ putStr
main1 = outputString'

-- Monadic bind
outputIO = foldr (\n accum -> putStrLn (fizzbuzz n) >> accum) (putStr "") [0..99]
main2 = outputIO

---- foldl' versions

-- Poorer performance than `main`
outputString'' = foldl' (\accum n -> accum ++ fizzbuzz n ++ "\n") "" [0..99] 
main3 = putStr outputString''

-- Tail-recursive strict-evaluated foldl', maybe better than foldr for monadic bind
outputIO' = foldl' (\accum n -> accum >> putStrLn (fizzbuzz n)) (putStr "") [0..99]
main4 = outputIO'