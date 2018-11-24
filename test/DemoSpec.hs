{-# LANGUAGE NumDecimals #-}

module DemoSpec (main, spec) where

import Test.Hspec
import Demo
import Data.List (sort)
import Control.Concurrent.Async (race_)
import Control.Concurrent (threadDelay)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Transitive closure" $ do
    it "rel0" $
      trancl rel0 [1] `shouldBe` [2,0,3,4,1]
    it "rel1" $
      trancl rel2 [22] `shouldBe` [33,44]
    it "rel2" $
      trancl rel2 [2,5] `shouldBe` [2,4,6,8,1,3,5,7,9]
    it "rel2 (incorrect)" $
      sort (trancl rel2 [42,8,9]) `shouldBe` [1,2,3,4,5,6,7,8,9,42]
    it "rel1" $
      sort (trancl rel1 [42,8,9]) `shouldBe` [1,2,4,5,7,8,9,10,11,13,14,16,17,20,21,22,26,28,32,34,40,42,52,64]
    it "handles non-terminating relations" . raceTest $
      take 10 (trancl rel3 [0]) `shouldBe` [1,2,3,4,5,6,7,8,9,10]

raceTest :: Expectation -> Expectation
raceTest = race_ (threadDelay 1e6 *> expectationFailure "Test took too long")
