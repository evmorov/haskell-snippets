class Point a where
  coord :: a -> (Float, Float)

data FloatPoint = FloatPoint Float Float
instance Point FloatPoint where
  coord (FloatPoint x y) = (x,y)

data IntPoint = IntPoint Int Int
instance Point IntPoint where
  coord (IntPoint x y) = (realToFrac x, realToFrac y)

main :: IO ()
main = do
  let fpoint = FloatPoint 0.1 0.2
  print $ coord fpoint
  let ipoint = IntPoint 1 2
  print $ coord ipoint
