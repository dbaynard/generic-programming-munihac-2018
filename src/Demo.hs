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
module Demo where

import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits

import Data.Function

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
    Datatype "Bool"
      (Plus
        (Constructor "False" One)
        (Constructor "True" One)
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
    Datatype "List"
      (Plus
        (Constructor "Nil" One)
        (Constructor "Cons" (Times (Const a) (Const (List a))))
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

instance GEnum Zero where
  genum = []

instance GEnum One where
  genum = [ () ]

instance (GEnum c1, GEnum c2) => GEnum (Plus c1 c2) where
  genum =
    map Left (genum @c1) ++ map Right (genum @c2)

instance (GEnum c1, GEnum c2) => GEnum (Times c1 c2) where
  genum =
    [ (x, y) | x <- genum @c1, y <- genum @c2 ]

instance MyEnum a => GEnum (Const a) where
  genum = enum

instance GEnum c => GEnum (Datatype n c) where
  genum = genum @c

instance GEnum c => GEnum (Constructor n c) where
  genum = genum @c

type family Run (c :: Code) :: Type where
  Run Zero              = Void
  Run One               = ()
  Run (Const a)         = a
  Run (Plus c1 c2)      = Either (Run c1) (Run c2)
  Run (Times c1 c2)     = (Run c1, Run c2)
  Run (Datatype _ c)    = Run c
  Run (Constructor _ c) = Run c















class GEq (c :: Code) where
  geq :: Run c -> Run c -> Bool

instance GEq Zero where
  geq x _ = absurd x

instance GEq One where
  geq () () = True

instance (GEq c1, GEq c2) => GEq (Plus c1 c2) where
  geq (Left x1) (Left x2) = geq @c1 x1 x2
  geq (Right y1) (Right y2) = geq @c2 y1 y2
  geq _ _ = False

instance (GEq c1, GEq c2) => GEq (Times c1 c2) where
  geq (x1, y1) (x2, y2) =
    geq @c1 x1 x2 && geq @c2 y1 y2

instance Eq a => (GEq (Const a)) where
  geq x y = x == y

instance GEq c => GEq (Datatype n c) where
  geq = geq @c

instance GEq c => GEq (Constructor n c) where
  geq = geq @c

class GShow (c :: Code) where
  gshow :: Run c -> String

instance GShow Zero where
  gshow x = absurd x

instance GShow One where
  gshow () = ""

instance (GShow c1, GShow c2) => GShow (Plus c1 c2) where
  gshow (Left x) = gshow @c1 x
  gshow (Right y) = gshow @c2 y

instance (GShow c1, GShow c2) => GShow (Times c1 c2) where
  gshow (x, y) = gshow @c1 x ++ " " ++ gshow @c2 y

instance Show a => GShow (Const a) where
  gshow x = "(" ++ show x ++ ")"

instance GShow c => GShow (Datatype n c) where
  gshow = gshow @c

instance (KnownSymbol n, GShow c) => GShow (Constructor n c) where
  gshow x = symbolVal (Proxy @n) ++ " " ++ gshow @c x


rel0 :: Int -> [Int]
rel0 0 = [0]
rel0 1 = [2]
rel0 2 = [0,3]
rel0 3 = [4]
rel0 4 = [1]
rel0 _ = []

rel1 0 = [0]
rel1 n
  | even n = [n, n `div` 2]
  | otherwise = [3*n + 1, n]

rel2 11 = [22]
rel2 22 = [33]
rel2 33 = [44]
rel2 n
  | even n, n>=1, n<=9 = [2,4,6,8]
  | odd n, n>=1, n<=9 = [1,3,5,7,9]
  | otherwise = [n]

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
