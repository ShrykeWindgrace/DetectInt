{-# LANGUAGE TypeApplications #-}

module Main where

import           Lib
import           Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "detect int" $ do
            it "newtype" $
                hasInt @NInt `shouldBe` True
            it "datatype" $
                hasInt @OnlyInt `shouldBe` True
            it "product" $
                hasInt @BoolAndInt `shouldBe` True
            it "sum" $
                hasInt @BoolOrInt `shouldBe` True
            it "self-recursive" $
                hasInt @RecInt `shouldBe` True
            it "self-recursive, no Ints" $
                hasInt @NoInt `shouldBe` False

