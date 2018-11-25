--
-- An Introduction to Datatype-Generic Programming in Haskell
--
-- Andres Löh
-- MuniHac, 2018-11-17
--

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Demo where

import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits
import GHC.Exts (IsList)

import Control.Monad.State
import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.Maybe (maybeToList)

-- import Data.Function

data Code =
    Zero
  | One
  | Const Type
  | Plus Code Code
  | Times Code Code
  | Datatype Symbol Code
  | Constructor Symbol Code

type Rep a = Run (CodeOf a)

class Generic a where
  type CodeOf a :: Code

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic Bool where
  type CodeOf Bool =
    'Datatype "Bool"
      ('Plus
        ('Constructor "False" 'One)
        ('Constructor "True" 'One)
      )

  from :: Bool -> Rep Bool
  from False = Left ()
  from True  = Right ()

  to :: Rep Bool -> Bool
  to (Left ()) = False
  to (Right ()) = True

data List a = Nil | Cons a (List a)
  deriving (MyEnum, Eq, Show) via FromGeneric (List a)

instance Generic (List a) where
  type CodeOf (List a) =
    'Datatype "List"
      ('Plus
        ('Constructor "Nil" 'One)
        ('Constructor "Cons" ('Times ('Const a) ('Const (List a))))
      )

  from :: List a -> Rep (List a)
  from Nil = Left ()
  from (Cons x xs) = Right (x, xs)

  to :: Rep (List a) -> List a
  to (Left ()) = Nil
  to (Right (x, xs)) = Cons x xs

class MyEnum a where
  enum :: [a]

newtype FromGeneric a = FromGeneric a

instance (Generic a, GEnum (CodeOf a)) => MyEnum (FromGeneric a) where
  enum = map (FromGeneric . to) (genum @(CodeOf a))

instance (Generic a, GEq (CodeOf a)) => Eq (FromGeneric a) where
  FromGeneric x1 == FromGeneric x2 = geq @(CodeOf a) (from x1) (from x2)

instance (Generic a, GShow (CodeOf a)) => Show (FromGeneric a) where
  show (FromGeneric x) = gshow @(CodeOf a) (from x)

deriving via FromGeneric Bool instance MyEnum Bool

{-
instance MyEnum Bool where
  enum = map to (genum @(CodeOf Bool))
-}

class GEnum (c :: Code) where
  genum :: [Run c]

instance GEnum 'Zero where
  genum = []

instance GEnum 'One where
  genum = [ () ]

instance (GEnum c1, GEnum c2) => GEnum ('Plus c1 c2) where
  genum =
    map Left (genum @c1) ++ map Right (genum @c2)

instance (GEnum c1, GEnum c2) => GEnum ('Times c1 c2) where
  genum =
    [ (x, y) | x <- genum @c1, y <- genum @c2 ]

instance MyEnum a => GEnum ('Const a) where
  genum = enum

instance GEnum c => GEnum ('Datatype n c) where
  genum = genum @c

instance GEnum c => GEnum ('Constructor n c) where
  genum = genum @c

type family Run (c :: Code) :: Type where
  Run 'Zero              = Void
  Run 'One               = ()
  Run ('Const a)         = a
  Run ('Plus c1 c2)      = Either (Run c1) (Run c2)
  Run ('Times c1 c2)     = (Run c1, Run c2)
  Run ('Datatype _ c)    = Run c
  Run ('Constructor _ c) = Run c















class GEq (c :: Code) where
  geq :: Run c -> Run c -> Bool

instance GEq 'Zero where
  geq x _ = absurd x

instance GEq 'One where
  geq () () = True

instance (GEq c1, GEq c2) => GEq ('Plus c1 c2) where
  geq (Left x1) (Left x2) = geq @c1 x1 x2
  geq (Right y1) (Right y2) = geq @c2 y1 y2
  geq _ _ = False

instance (GEq c1, GEq c2) => GEq ('Times c1 c2) where
  geq (x1, y1) (x2, y2) =
    geq @c1 x1 x2 && geq @c2 y1 y2

instance Eq a => (GEq ('Const a)) where
  geq x y = x == y

instance GEq c => GEq ('Datatype n c) where
  geq = geq @c

instance GEq c => GEq ('Constructor n c) where
  geq = geq @c

class GShow (c :: Code) where
  gshow :: Run c -> String

instance GShow 'Zero where
  gshow x = absurd x

instance GShow 'One where
  gshow () = ""

instance (GShow c1, GShow c2) => GShow ('Plus c1 c2) where
  gshow (Left x) = gshow @c1 x
  gshow (Right y) = gshow @c2 y

instance (GShow c1, GShow c2) => GShow ('Times c1 c2) where
  gshow (x, y) = gshow @c1 x ++ " " ++ gshow @c2 y

instance Show a => GShow ('Const a) where
  gshow x = "(" ++ show x ++ ")"

instance GShow c => GShow ('Datatype n c) where
  gshow = gshow @c

instance (KnownSymbol n, GShow c) => GShow ('Constructor n c) where
  gshow x = symbolVal (Proxy @n) ++ " " ++ gshow @c x


rel0 :: Int -> [Int]
rel0 0 = [0]
rel0 1 = [2]
rel0 2 = [0,3]
rel0 3 = [4]
rel0 4 = [1]
rel0 _ = []

rel1 :: Int -> [Int]
rel1 0 = [0]
rel1 n
  | even n = [n, n `div` 2]
  | otherwise = [3*n + 1, n]

rel2 :: Int -> [Int]
rel2 11 = [22]
rel2 22 = [33]
rel2 33 = [44]
rel2 n
  | even n, n>=1, n<=9 = [2,4,6,8]
  | odd n, n>=1, n<=9 = [1,3,5,7,9]
  | otherwise = [n]

rel3 :: Int -> [Int]
rel3 i = [i + 1]

trancl :: forall a . Eq a => (a -> [a]) -> [a] -> [a]
trancl r x0 = go [] x0
  where

    flatMapped :: [a] -> [a]
    flatMapped xs = concatMap r xs

    filtered :: [a] -> [a] -> [a]
    filtered seen xs = filter (`notElem` seen) xs

    tgthr :: [a] -> [a] -> [a]
    tgthr seen = filtered seen . flatMapped

    go out [] = out
    go out inp = let next = tgthr out inp in go (out ++ next) next

    {-
     -go1 = tgthr [] [1]
     -go2 = tgthr [2] [2]
     -go3 = tgthr [2,0,3] [0,3]
     -go4 = tgthr [2,0,3,4] [4]
     -go5 = tgthr [2,0,3,4,1] [1]
     -go6 = tgthr [2,0,3,4,1] []
     -}

diagonalProduct :: [a] -> [b] -> [(a,b)]
diagonalProduct [] _ = []
diagonalProduct _ [] = []
diagonalProduct (a:as) (b:bs) = (a,b) : merge (diagonalProduct (a:as) bs) (diagonalProduct as (b:bs))
  where
    merge :: [c] -> [c] -> [c]
    merge [] xs = xs
    merge xs [] = xs
    merge (x:xs) (y:ys) = x : y : merge xs ys

{-
 -mergeWith :: (a -> b -> c) -> [a] -> [b] -> [c]
 -mergeWith f (a:as) (b:bs) = f a b
 -  where
 -    nextRight (x:xs) (y:ys) = 
 -}

newtype RevList a = R [a]
  deriving stock (Eq, Show)
  deriving newtype (IsList)

newtype ForList a = F [a]
  deriving stock (Eq, Show)
  deriving newtype (IsList)

data Zipper a = Z (RevList a) a (ForList a)
  deriving stock (Eq, Show)

mkZipper :: [a] -> Maybe (Zipper a)
mkZipper [] = Nothing
mkZipper (x:xs) = Just $ Z [] x (F xs)

data Diagonal a b
  = Across (Zipper a) (Zipper b)
  | Down (Zipper a) (Zipper b)
  deriving stock (Eq, Show)
infixr 6 `Across`
infixr 6 `Down`

diagonal :: [a] -> [b] -> [(a,b)]
diagonal = curry $ runDiag <=< maybeToList . uncurry mkDiagonal

mkDiagonal :: [a] -> [b] -> Maybe (Diagonal a b)
mkDiagonal = curry $ fmap (uncurry Down) . azip . bimap mkZipper mkZipper

azip :: Applicative f => (f a, f b) -> f (a, b)
azip = uncurry $ (<*>) . fmap (,)
{-# INLINE azip #-}

runDiag :: Diagonal a b -> [(a,b)]
runDiag = fix runDiag'

runDiag'
  :: (Diagonal a b -> [(a,b)])
  -> Diagonal a b -> [(a,b)]
runDiag' r d0 = case nextDiag d0 of
  (pair, Nothing) -> [pair]
  (_, Just d1) -> r d1

type NextDiag a b = ((a,b), Maybe (Diagonal a b))

nextDiag :: Diagonal a b -> NextDiag a b
-- empty case
nextDiag (Z _ b [] `Across` Z _ y []) = ((b,y), Nothing)
nextDiag (Z _ b [] `Down` Z _ y []) = ((b,y), Nothing)

-- initial case
-- Going Down, shuffle left forwards and flip
nextDiag (Z [] b (F (d:c)) `Down` r@(Z [] y _)) = ((b,y), Just (Z (R [b]) d (F c) `Across` r))

-- Corner cases (opposites empty)
-- Going down, when there is no reverse list on the left and no forward
-- list on the right, drop the focus on the left and shuffle the right list
-- backwards
nextDiag (Z [] b (F (d:c)) `Down` Z (R (w:x)) y []) = ((b,y), Just (Z [] d (F c) `Down` Z (R x) w (F [y])))
-- Going down, when there is no reverse list on the right and no forward
-- list on the left, drop the focus on the right and flip the Zipper
nextDiag (l@(Z _ b []) `Down` Z [] y (F (w:z))) = ((b,y), Just (l `Across` Z [] w (F z)))
-- Going across, when there is no reverse list on the right and no forward
-- list on the left, drop the focus on the right and shuffle the left list
-- backwards
nextDiag ((Z (R (d:a)) b []) `Across` Z [] y (F (w:z))) = ((b,y), Just (Z (R a) d (F [b]) `Across` Z [] w (F z)))
-- Going across, when there is no reverse list on the left and no forward
-- list on the right, drop the focus on the left and flip the Zipper
nextDiag (Z [] b (F (d:c)) `Across` r@(Z _ y [])) = ((b,y), Just (Z [] d (F c) `Down` r))

-- Going across, when there is no reverse list for right, shuffle left
-- focus backwards and right forwards
nextDiag (Z (R (d:a)) b (F c) `Across` Z [] y (F (w:z))) = ((b,y), Just (Z (R a) d (F (b:c)) `Across` Z (R [y]) w (F z)))
-- Actually, it doesn't matter what the right list is here, making the above
-- redundant
nextDiag (Z (R (d:a)) b (F c) `Across` Z (R x) y (F (w:z))) = ((b,y), Just (Z (R a) d (F (b:c)) `Across` Z (R (y:x)) w (F z)))
-- Going across, when there is no reverse list for left, flip Zipper and
-- shuffle right focus forwards
nextDiag (l@(Z [] b _) `Across` Z (R x) y (F (w:z))) = ((b,y), Just (l `Down` Z (R (y:x)) w (F z)))

-- Going down, when there is no reverse list for right, flip Zipper and
-- shuffle left focus forwards
nextDiag (Z (R a) b (F (d:c)) `Down` r@(Z [] y _)) = ((b,y), Just (Z (R (b:a)) d (F c) `Across` r))
-- Going down, when there is no reverse list for left, shuffle left focus
-- forwards and right backwards
nextDiag (Z [] b (F (d:c)) `Down` Z (R (w:x)) y (F z)) = ((b,y), Just (Z (R [b]) d (F c) `Down` Z (R x) w (F (y:z))))
-- Actually, it doesn't matter what the left list is here, making the above
-- redundant
nextDiag (Z (R a) b (F (d:c)) `Down` Z (R (w:x)) y (F z)) = ((b,y), Just (Z (R (b:a)) d (F c) `Down` Z (R x) w (F (y:z))))
