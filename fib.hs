import Flow

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib <| n - 1) + (fib <| n - 2)

fib2 :: Int -> Int
fib2 n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = (fib2 $ n - 1) + (fib2 $ n - 2)

collatz :: Int -> Int
collatz n
    | even n = n `div` 2
    | odd n = 3 * n + 1

collatzAppend :: [ Int ] -> [ Int ]
collatzAppend [] = []
collatzAppend nums@(1:_) = nums
collatzAppend nums@(n:_) = collatz n : nums |> collatzAppend

singleton :: a -> [ a ]
singleton a = [ a ]

collatzProgression :: Int -> [ Int ]
collatzProgression = singleton .> collatzAppend .> reverse

people :: [ ( String, Float, Float ) ] -- name, height in cm, mass in kg
people = [ ("Tim", 191, 100), ("Jane", 175, 88.8), ("Zed", 180, 150)]