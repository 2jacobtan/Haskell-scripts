import Data.Function ( (&), fix )
import Data.List (foldl')

-- top-down
fib n = snd $ go n
  where
    -- go :: Int -> (Int,Int)
    go 0 = (0,0)
    go 1 = (0,1)
    go x = (b,a+b)
      where
        (a,b) = go (x - 1)

-- bottom-up
-- tail recursive
-- from https://www.youtube.com/watch?v=_JtPhF8MshA&t=13m12s
fib' n = go n (0,1)
  where
    go 0 (a,_) = a
    go 1 (_,b) = b
    go x (a,b) = go (x-1) (b,a+b)

-- tail recursive strict left-fold
fib'' 0 = 0
fib'' 1 = 1
fib'' n = snd $ foldl' (\(a,b) _ -> (b,a+b)) (0,1) [2..n]
-- in one line
fib''' n = fst $ foldl' (\(a,b) _ -> (b,a+b)) (0,1) [1..n]

-- from https://www.reddit.com/r/haskell/comments/gsascr/tail_recursion_explained_computerphile/fs5yqhe/
fibs = fix $ (0:) . scanl (+) 1
fib4 = (fibs !!)

main0 = do
  putStrLn $ show $ map fib [0..7]
  putStrLn $ show $ map fib' [0..7]

main1 =
  [fib,fib',fib'',fib''',fib4]
  & map (\f -> map f [0..7])
  & mapM_ (\l -> l & show & putStrLn)

main = main1