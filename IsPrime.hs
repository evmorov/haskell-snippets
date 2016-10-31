import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prime" $ do
    describe "isPrime1" $ do
      it "4" $ isPrime1 4 `shouldBe` False
      it "5" $ isPrime1 5 `shouldBe` True
      it "41" $ isPrime1 41 `shouldBe` True
      it "661" $ isPrime1 661 `shouldBe` True

    describe "isPrime2" $ do
      it "4" $ isPrime1 4 `shouldBe` False
      it "5" $ isPrime2 5 `shouldBe` True
      it "41" $ isPrime2 41 `shouldBe` True
      it "661" $ isPrime2 661 `shouldBe` True

    describe "isPrime3" $ do
      it "4" $ isPrime1 4 `shouldBe` False
      it "5" $ isPrime3 5 `shouldBe` True
      it "41" $ isPrime3 41 `shouldBe` True
      it "661" $ isPrime3 661 `shouldBe` True

    describe "isPrime4" $ do
      it "4" $ isPrime1 4 `shouldBe` False
      it "5" $ isPrime4 5 `shouldBe` True
      it "41" $ isPrime4 41 `shouldBe` True
      it "661" $ isPrime4 661 `shouldBe` True

    describe "isPrime5" $ do
      it "4" $ isPrime1 4 `shouldBe` False
      it "5" $ isPrime5 5 `shouldBe` True
      it "41" $ isPrime5 41 `shouldBe` True
      it "661" $ isPrime5 661 `shouldBe` True

isPrime1 :: Integer -> Bool
isPrime1 x = null [y | y <- [2 .. floor (sqrt (fromIntegral x))], x `mod` y == 0]

isPrime2 x = null (filter (\y -> x `mod` y == 0) (takeWhile (\y -> y * y <= x) [2 ..]))

isPrime3 x = null $ filter (\y -> x `mod` y == 0) $ takeWhile (\y -> y * y <= x) [2 ..]

isPrime4 x = null $ filter divisible $ takeWhile notTooBig [2 ..]
  where
    divisible y = x `mod` y == 0
    notTooBig y = y * y <= x

isPrime5 x = not $ any divisible $ takeWhile notTooBig [2 ..]
  where
    divisible y = x `mod` y == 0
    notTooBig y = y * y <= x
