class YesNo a where
  yesno :: a -> Bool

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

-- Huh? What's id? It's just a standard library function that takes a parameter
-- and returns the same thing, which is what we would be writing here anyway.
instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

yesNoExample = do
  print $ yesno []
  print $ yesno [1, 2, 3]

  -- http://stackoverflow.com/questions/16824047/haskell-yesno-type-class-why-integer
  print $ yesno (0 :: Int)
  print $ yesno (1 :: Int)

  print $ yesno False
  print $ yesno True
  print $ yesno $ Nothing
  print $ yesno $ Just 0

  print $ yesnoIf [] "YEAH!" "NO!"
  print $ yesnoIf True "YEAH!" "NO!"


class Point a where
    coord :: a -> (Float, Float)

data FloatPoint = FloatPoint Float Float
instance Point FloatPoint where
    coord (FloatPoint x y) = (x,y)

data IntPoint = IntPoint Int Int
instance Point IntPoint where
    coord (IntPoint x y) = (realToFrac x, realToFrac y)

pointExample = do
  let fpoint = FloatPoint 0.1 0.2
  print $ coord fpoint
  let ipoint = IntPoint 1 2
  print $ coord ipoint

main = do
  yesNoExample
  pointExample
