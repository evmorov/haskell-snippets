-- The head and tail functions from the Prelude are not
-- safe in the sense that they fail when called on an
-- empty list. We can define safe versions using Maybe

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:xs) = Just xs

main :: IO ()
main = do
  print $ myHead [1, 2, 3]
  print $ myTail [1, 2, 3]
