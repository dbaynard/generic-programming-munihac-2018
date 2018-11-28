{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Record where

import GHC.Generics (Generic)
import qualified GHC.Generics as G
import GHC.TypeLits

import Generics.Deriving.Enum
import Generics.Deriving.Eq

import Data.List (transpose, findIndex)
import Data.Proxy
import Data.Void

--------------------------------------------------------------------------------

type family (:::) (tag :: k) (label :: Symbol) :: *
infix 5 :::

type instance (:::) Void label = Void

data Shown

type instance (:::) Shown label = Proxy label

--------------------------------------------------------------------------------

-- From generic-deriving
toEnumDef :: (Generic a, MyGEnum' (G.Rep a)) => Int -> a
toEnumDef i = let l = mygenum'
                  in if (length l > i)
                      then G.to (l !! i)
                       else error "toEnum: invalid index"

-- From generic-deriving
fromEnumDef :: (GEq a, Generic a, MyGEnum' (G.Rep a))
                => a -> Int
fromEnumDef x = case geq x `findIndex` (G.to <$> mygenum') of
      Nothing -> error "fromEnum: no corresponding index"
      Just i  -> i

class MyGEnum a where
  mygenum :: [a]

  default mygenum :: (Generic a, MyGEnum' (G.Rep a)) => [a]
  mygenum = G.to <$> mygenum'

class MyGEnum' f where
  mygenum' :: [f a]

instance MyGEnum' G.V1 where
  mygenum' = []

instance MyGEnum' G.U1 where
  mygenum' = [G.U1]

instance (MyGEnum' a, MyGEnum' b) => MyGEnum' (a G.:+: b) where
  mygenum' = (G.L1 <$> mygenum' @a) ++ (G.R1 <$> mygenum' @b)

instance (MyGEnum' a, MyGEnum' b) => MyGEnum' (a G.:*: b) where
  mygenum' = uncurry (G.:*:) <$> mygenum' @a `simpleDiagonal` mygenum' @b

instance MyGEnum c => MyGEnum' (G.K1 i c) where
  mygenum' = G.K1 <$> mygenum @c

instance MyGEnum' a => MyGEnum' (G.M1 i t a) where
  mygenum' = G.M1 <$> mygenum' @a

simpleDiagonal :: [a] -> [b] -> [(a,b)]
simpleDiagonal xs0 ys0 = go [] $ makeCartesian xs0 ys0
  where
    makeCartesian xs ys = [[(x,y) | x <- xs] | y <- ys]

    go :: forall c . [[c]] -> [[c]] -> [c]
    go upper lower =
        -- generate the upper left half and the diagonal, by sticking on
        -- the head of each list in upper
      [ h :: c | h:_ <- upper :: [[c]]] ++ case lower of
        -- transpose the remaining lists
        [] -> concat . transpose $ upper'
        -- Recurse, by sticking on the new full-length list
        -- Each round shrinks each row in upper' by 1
        row:lower' -> go (row:upper') lower'
        where
          -- take the tails of all the lists in upper
          upper' :: [[c]] = [ t :: [c] | _:t <- upper :: [[c]] ]

--------------------------------------------------------------------------------

newtype AsEq a = AsEq a

instance (Generic a, GEq' (G.Rep a)) => Eq (AsEq a) where
  AsEq x == AsEq y = x `geqdefault` y

instance (Generic a, GEq' (G.Rep a)) => GEq (AsEq a) where
  AsEq x `geq` AsEq y = x `geqdefault` y

newtype AsEnum a = AsEnum a

instance (Generic a, GEq a, MyGEnum' (G.Rep a)) => Enum (AsEnum a) where
  toEnum = AsEnum . toEnumDef
  fromEnum (AsEnum x) = fromEnumDef x

instance (Generic a, GEq a, MyGEnum' (G.Rep a)) => MyGEnum (AsEnum a) where
  mygenum = AsEnum . G.to <$> mygenum'

--------------------------------------------------------------------------------

data CustomEnum_ w
  = First (w ::: "The first")
  | Second (w ::: "The second")
  deriving stock (Generic)

type CustomEnum = CustomEnum_ Void

firsty :: CustomEnum
firsty = First undefined

mkShown :: CustomEnum -> CustomEnum_ Shown
mkShown (First _) = First Proxy
mkShown (Second _) = Second Proxy

instance Show CustomEnum where
  show = show @(CustomEnum_ Shown) . mkShown

instance Show (CustomEnum_ Shown) where
  show (First p) = symbolVal p
  show (Second p) = symbolVal p

deriving via (AsEq CustomEnum) instance Eq CustomEnum
deriving via (AsEq CustomEnum) instance GEq CustomEnum

deriving via (AsEnum CustomEnum) instance MyGEnum CustomEnum
deriving via (AsEnum CustomEnum) instance Enum CustomEnum
