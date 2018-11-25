{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DemoSpec (main, spec, raceTest) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Demo
import Data.List (sort)
import Control.Concurrent.Async (race_)
import Control.Concurrent (threadDelay)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE (toList)
import Test.QuickCheck.Instances ()
import Control.Applicative

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
    {-
     -it "handles non-terminating relations" . raceTest $
     -  take 10 (trancl rel3 [0]) `shouldBe` [1,2,3,4,5,6,7,8,9,10]
     -}

  describe "Diagonalization" $ do
    prop "makes the right initial diagonal" $ \as'@((a :: Int) :| as) bs'@((b :: Int) :| bs) -> do
      mkDiagonal (NE.toList as') (NE.toList bs') === Just (Z [] a (F as) `Down` Z [] b (F bs))
    {-
     -it "correct diagonalizations 1×1" $ do
     -  diagonal [1..1] [1..1] `shouldBe` [(1 :: Int,1 :: Int)]
     -it "correct diagonalizations 2×2" $ do
     -  diagonal [1..2] [1..2] `shouldBe` [(1,1),(1,2),(2,1),(2,2)]
     -it "correct diagonalizations 3×3" $ do
     -  diagonal [1..3] [1..3] `shouldBe` [(1,1),(1,2),(2,1),(3,1),(2,2),(1,3)]
     -}
    it "makes the right specific initial diagonal" $ do
      mkDiagonal ['A'..'E'] [0..4 :: Int] `shouldBe` Just (Z [] 'A' ['B'..'E'] `Down` Z [] 0 [1..4])
    sequence_ $ testingDiagonalExample <$> ZipList [0..] <*> ZipList ordered <*> ZipList (tail ordered)

testingDiagonalExample :: (Eq a, Eq b, Show a, Show b) => Int -> (NextDiag a b) -> (NextDiag a b) -> Spec
testingDiagonalExample n (_, Just ex) res = it (unwords ["Example iteration", show n, show (fst res)]) $
  nextDiag ex `shouldBe` res
testingDiagonalExample _ _ _ = error "Invalid test input"

ordered :: [NextDiag Char Int]
ordered =
  [ (undefined, Just ( Z [] 'A' ['B'..'E'] `Down` Z [] 0 [1..4]))
  , (('A',0), Just (Z ['A'] 'B' ['C'..'E'] `Across` Z [] 0 [1..4]))
  , (('B',0), Just (Z [] 'A' ['B'..'E'] `Across` Z [0] 1 [2..4]))
  , (('A',1), Just (Z [] 'A' ['B'..'E'] `Down` Z [1,0] 2 [3..4]))
  , (('A',2), Just (Z ['A'] 'B' ['C'..'E'] `Down` Z [0] 1 [2..4]))
  , (('B',1), Just (Z ['B','A'] 'C' ['D'..'E'] `Down` Z [] 0 [1..4]))
  , (('C',0), Just (Z ['C','B'..'A'] 'D' ['E'] `Across` Z [] 0 [1..4]))
  , (('D',0), Just (Z ['B','A'] 'C' ['D'..'E'] `Across` Z [0] 1 [2..4]))
  , (('C',1), Just (Z ['A'] 'B' ['C'..'E'] `Across` Z [1,0] 2 [3..4]))
  , (('B',2), Just (Z [] 'A' ['B'..'E'] `Across` Z [2,1..0] 3 [4]))
  , (('A',3), Just (Z [] 'A' ['B'..'E'] `Down` Z [3,2..0] 4 []))
  , (('A',4), Just (Z [] 'B' ['C'..'E'] `Down` Z [2,1..0] 3 [4]))
  , (('B',3), Just (Z ['B'] 'C' ['D'..'E'] `Down` Z [1,0] 2 [3..4]))
  , (('C',2), Just (Z ['C','B'] 'D' ['E'] `Down` Z [0] 1 [2..4]))
  , (('D',1), Just (Z ['D','C'..'B'] 'E' [] `Down` Z [] 0 [1..4]))
  , (('E',0), Just (Z ['D','C'..'B'] 'E' [] `Across` Z [] 1 [2..4]))
  , (('E',1), Just (Z ['C','B'] 'D' ['E'] `Across` Z [] 2 [3..4]))
  , (('D',2), Just (Z ['B'] 'C' ['D'..'E'] `Across` Z [2] 3 [4]))
  , (('C',3), Just (Z [] 'B' ['C'..'E'] `Across` Z [3,2] 4 []))
  , (('B',4), Just (Z [] 'C' ['D'..'E'] `Down` Z [3,2] 4 []))
  , (('C',4), Just (Z [] 'D' ['E'] `Down` Z [2] 3 [4]))
  , (('D',3), Just (Z ['D'] 'E' [] `Down` Z [] 2 [3,4]))
  , (('E',2), Just (Z ['D'] 'E' [] `Across` Z [] 3 [4]))
  , (('E',3), Just (Z [] 'D' ['E'] `Across` Z [] 4 []))
  , (('D',4), Just (Z [] 'E' [] `Down` Z [] 4 []))
  , (('E',4), Nothing)
  ]

raceTest :: Expectation -> Expectation
raceTest = race_ (threadDelay 1e6 *> expectationFailure "Test took too long")

