import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Elapsed Test" $ do
    it "elapsed in seconds only" $ do
      elapsed "2018-06-25T21:53:35" "2018-06-25T21:53:46" `shouldBe` (11 :: Int)

    it "elapsed in mins and seconds" $ do
      elapsed "2018-06-25T21:51:35" "2018-06-25T21:53:46" `shouldBe` (131 :: Int)

    it "elapsed in hours, mins and seconds" $ do
      elapsed "2018-06-25T19:51:35" "2018-06-25T21:53:46" `shouldBe` (7331 :: Int)

    it "elapsed in days, hours, mins and seconds" $ do
      elapsed "2018-06-23T19:51:35" "2018-06-25T21:53:46" `shouldBe` (180131 :: Int)

    it "elapsed in 31day, month, days, hours, mins and seconds" $ do
      elapsed "2018-05-23T19:51:35" "2018-06-25T21:53:46" `shouldBe` (2858531 :: Int)

    it "elapsed in 33day, month, days, hours, mins and seconds" $ do
      elapsed "2018-04-23T19:51:35" "2018-05-25T21:53:46" `shouldBe` (2772131 :: Int)

    it "elapsed in 28day, month, days, hours, mins and seconds" $ do
      elapsed "2018-02-23T19:51:35" "2018-03-25T21:53:46" `shouldBe` (2599331 :: Int)

    it "elapsed in multiple months, days, hours, mins and seconds" $ do
      elapsed "2018-02-23T19:51:35" "2018-04-25T21:53:46" `shouldBe` (5277731 :: Int)

    it "elapsed in years, multiple months, days, hours, mins and seconds" $ do
        elapsed "2016-03-23T19:51:35" "2018-04-25T21:53:46" `shouldBe` (65930531 :: Int)

    it "elapsed with leap year at start boundary excl feb, multiple month, days, hours, mins and seconds diff" $ do
        elapsed "2016-03-23T19:51:35" "2018-04-25T21:53:46" `shouldBe` (65930531 :: Int)

    it "elapsed with leap year at start boundary incl feb, multiple month, days, hours, mins and seconds diff" $ do
        elapsed "2016-02-23T19:51:35" "2018-04-25T21:53:46" `shouldBe` (68436131 :: Int)

    it "elapsed with leap year at end boundary excl feb, multiple month, days, hours, mins and seconds diff" $ do
          elapsed "2015-02-23T19:51:35" "2016-02-25T21:53:46" `shouldBe` (31716131 :: Int)

    it "elapsed with leap year at end boundary incl feb, multiple month, days, hours, mins and seconds diff" $ do
          elapsed "2015-02-23T19:51:35" "2016-04-25T21:53:46" `shouldBe` (36900131 :: Int)

    it "elapsed with multiple leap years in between, multiple month, days, hours, mins and seconds diff" $ do
          elapsed "2007-02-23T19:51:35" "2016-04-25T21:53:46" `shouldBe` (289360931 :: Int)
