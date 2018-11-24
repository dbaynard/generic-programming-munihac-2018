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
      mkDiagonal (NE.toList as') (NE.toList bs') === Just (Z [] a (F as) `Diagonal` Z [] b (F bs))
    it "correct diagonalizations 1×1" $ do
      diagonal [1..1] [1..1] `shouldBe` [(1 :: Int,1 :: Int)]
    {-
     -it "correct diagonalizations 2×2" $ do
     -  diagonal [1..2] [1..2] `shouldBe` [(1,1),(1,2),(2,1),(2,2)]
     -it "correct diagonalizations 3×3" $ do
     -  diagonal [1..3] [1..3] `shouldBe` [(1,1),(1,2),(2,1),(3,1),(2,2),(1,3)]
     -}
    it "makes the right specific initial diagonal" $ do
      mkDiagonal [0..4 :: Int] [0..4 :: Int] `shouldBe` Just (Z [] 0 [1..4] `Diagonal` Z [] 0 [1..4])
    sequence_ $ testingDiagonalExample <$> ZipList [0..] <*> ZipList ordered <*> ZipList (tail ordered)

testingDiagonalExample :: Int -> (NextDiag Int Int) -> (NextDiag Int Int) -> Spec
testingDiagonalExample n (_, Just ex) res = it ("Example iteration " <> show n) $
  uncurry nextDiag ex `shouldBe` res
testingDiagonalExample _ _ _ = error "Invalid test input"

ordered :: [NextDiag Int Int]
ordered =
  [ (undefined, Just (False, Z [] 0 [1..4] `Diagonal` Z [] 0 [1..4]))
  , ((0,0), Just (True, Z [0] 1 [2..4] `Diagonal` Z [] 0 [1..4]))
  , ((1,0), Just (True, Z [] 0 [1..4] `Diagonal` Z [0] 1 [2..4]))
  , ((0,1), Just (False, Z [] 0 [1..4] `Diagonal` Z [1,0] 2 [3..4]))
  , ((0,2), Just (False, Z [0] 1 [2..4] `Diagonal` Z [0] 1 [2..4]))
  , ((1,1), Just (False, Z [1,0] 2 [3..4] `Diagonal` Z [] 0 [1..4]))
  , ((2,0), Just (True, Z [2,1..0] 3 [4] `Diagonal` Z [] 0 [1..4]))
  , ((3,0), Just (True, Z [1,0] 2 [3..4] `Diagonal` Z [0] 1 [2..4]))
  , ((2,1), Just (True, Z [0] 1 [2..4] `Diagonal` Z [1,0] 2 [3..4]))
  , ((1,2), Just (True, Z [] 0 [1..4] `Diagonal` Z [2,1..0] 3 [4]))
  , ((0,3), Just (False, Z [] 0 [1..4] `Diagonal` Z [3,2..0] 4 []))
  , ((0,4), Just (False, Z [] 1 [2..4] `Diagonal` Z [2,1..0] 3 [4]))
  , ((1,3), Just (False, Z [1] 2 [3..4] `Diagonal` Z [1,0] 2 [3..4]))
  , ((2,2), Just (False, Z [2,1] 3 [4] `Diagonal` Z [0] 1 [2..4]))
  , ((3,1), Just (False, Z [3,2..1] 4 [] `Diagonal` Z [] 0 [1..4]))
  , ((4,0), Just (True, Z [3,2..1] 4 [] `Diagonal` Z [] 1 [2..4]))
  , ((4,1), Just (True, Z [2,1] 3 [4] `Diagonal` Z [1] 2 [3..4]))
  --, ((3,2), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((2,3), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((1,4), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((2,4), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((3,3), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((4,2), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((4,3), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((3,4), Just (True, Z [3,2..0] 4 [] `Diagonal` Z [] 0 [1..4]))
  --, ((4,4), Nothing)
  ]

raceTest :: Expectation -> Expectation
raceTest = race_ (threadDelay 1e6 *> expectationFailure "Test took too long")
