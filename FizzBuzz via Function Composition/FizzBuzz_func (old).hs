-- from https://www.youtube.com/watch?v=FyCYva9DhsI&t=21m03s
-- NDC conference talk: Clean Coders Hate What Happens to Your Code When You Use These Enterprise Programming Tricks

-- import Data.List (foldl')

fizzbuzz :: Integer -> String
fizzbuzz n = fizz (buzz id) $ show n
  where
    --test :: Integer -> String -> (String -> String) -> (String -> String)
    test d s x = case n `mod` d of
      0 -> \_ -> s ++ x ""
      _ -> x
    fizz x = test 3 "Fizz" x
    buzz x = test 5 "Buzz" x

outputString = foldr (\n accum -> fizzbuzz n ++ "\n" ++ accum) "" [0..99] 

outputIO = foldr (\n accum -> putStrLn (fizzbuzz n) >> accum) (putStr "") [0..99]

main = putStr outputString
main2 = outputIO

-- try at https://repl.it/@JacobTan2/BossyLastingUnits