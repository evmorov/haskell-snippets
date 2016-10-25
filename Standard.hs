main = do
  print $ head1 ['x', 'y', 'z']
  print $ sum1 [10, 20, 70]
  print $ sum2 [10, 20, 70]
  print $ max1 1 2
  print $ fst1 (6, 5)
  print $ zip1 [1, 2, 3, 4] ['a', 'b']
  print $ unzip1 [(1,'a'),(2,'b')]
  print $ map1 (*5) [1, 2, 3]
  print $ map2 (*5) [1, 2, 3]
  print $ filter1 (\x -> x < 5) [1, 2, 8, 9]
  print $ filter2 (<5) [1, 2, 8, 9]
  print $ filter3 (<5) [1, 2, 8, 9]
  print $ filter4 (<5) [1, 2, 8, 9]
  print $ length1 [1, 2, 3]
  print $ length2 [1, 2, 3]
  print $ length3 [1, 2, 3]
  print $ length4 [1, 2, 3]
  print $ length5 [1, 2, 3]

head1 :: [a] -> a
head1 (x:xs) = x

sum1 :: Num a => [a] -> a
sum1 ys =
  let sum1' [] total = total
      sum1' (x:xs) total = sum1' xs (total+x)
  in sum1' ys 0

sum2 ys = sum2' ys 0
  where
    sum2' [] total = total
    sum2' (x:xs) total = sum2' xs (total+x)

max1 :: Ord a => a -> a -> a
max1 x y
  | x > y = x
  | otherwise = y

fst1 :: (a, b) -> a
fst1 (x, y) = x

zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] ys = []
zip1 xs [] = []
zip1 (x:xs) (y:ys) = (x, y) : zip1 xs ys

unzip1 :: [(a, b)] -> ([a], [b])
unzip1 [] = ([], [])
unzip1 ((a, b):rest) =
  let (as, bs) = unzip1 rest
  in (a:as, b:bs)

map1:: (a -> b) -> [a] -> [b]
map1 f xs = [ f x | x <- xs ]

map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 pred lst
  | null lst = []
  | otherwise =
    if pred x
      then x : filter1 pred xs
      else filter1 pred xs
  where
    x:xs = lst

filter2 pred [] = []
filter2 pred (x:xs)
  | pred x = x : filter2 pred xs
  | otherwise = filter2 pred xs

filter3 p xs = [ x | x <- xs, p x ]

filter4 p [] = []
filter4 p (x:xs) = if p x then x : filter p xs else filter p xs

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

length2 lst =
  if lst == []
    then 0
    else let x:xs = lst
         in 1 + length2 xs

length3 lst
  | lst == [] = 0
  | otherwise =
    let x:xs = lst
    in 1 + length3 xs

length4 = _length4
  where
    _length4 [] = 0
    _length4 (x:xs') = 1 + _length4 xs'

length5 xs =
  case xs of
    [] -> 0
    (x:xs) -> 1 + length5 xs
