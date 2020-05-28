fib n = snd $ go n
  where
    -- go :: Int -> (Int,Int)
    go 0 = (0,0)
    go 1 = (0,1)
    go x = (b,(a+b))
      where
        (a,b) = go (x - 1)

-- tail recursive
fib' n = go n (0,1)
  where
    go 0 (a,b) = a
    go 1 (a,b) = b
    go x (a,b) = go (x-1) (b,a+b)