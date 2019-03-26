module Main where

import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Testing caesar" $ do

    it "Shifts correctly a simple string" $ do
      caesar "abcdefg" 1 `shouldBe` "BCDEFGH"

    it "Shifts correctly a lazy list of chars" $ do
      caesar ['a'..'d'] 2 `shouldBe` "CDEF"

    it "Shifts correctly a string containing spaces" $ do
      caesar "hello world" 5 `shouldBe` "MJQQTBTWQI"

    it "Shifts correctly a string containing non alphabetic chars" $ do
      caesar "This * a _string##§§" 10 `shouldBe` "DRSCKCDBSXQ"

    it "Can encode a simple string" $ do
      caesar "HELLOWORLD" 100 `shouldBe` "DAHHKSKNHZ"

    it "Can decode the previous encoded string back" $ do
      caesar "DAHHKSKNHZ" (-100) `shouldBe` "HELLOWORLD"
    
    it "Can shift a string up to 1000 times" $ do
      caesar "*I Am a very encoded $string!!!" 1000 `shouldBe` "UMYMHQDKQZOAPQPEFDUZS"

    it "Can shift a string up to 10000 times" $ do
      caesar "I love (to be) lazy (evaluated)!" 10000 `shouldBe` "YBELUJERUBQPOULQBKQJUT"