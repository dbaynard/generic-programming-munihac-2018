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

module Record where

import GHC.Generics (Generic)
import qualified GHC.Generics as G
import GHC.TypeLits

import Generics.Deriving.Enum
import Generics.Deriving.Eq

import Data.Proxy
import Data.Void

--------------------------------------------------------------------------------

type family (:::) (tag :: k) (label :: Symbol) :: *
infix 5 :::

type instance (:::) Void label = Void

data Shown

type instance (:::) Shown label = Proxy label

--------------------------------------------------------------------------------

newtype AsEq a = AsEq a

instance (Generic a, GEq' (G.Rep a)) => Eq (AsEq a) where
  AsEq x == AsEq y = x `geqdefault` y

instance (Generic a, GEq' (G.Rep a)) => GEq (AsEq a) where
  AsEq x `geq` AsEq y = x `geqdefault` y

newtype AsEnum a = AsEnum a

instance (Generic a, GEnum a, Enum' (G.Rep a)) => GEnum (AsEnum a) where
  genum = AsEnum <$> genum

instance (Generic a, GEq a, Enum' (G.Rep a)) => Enum (AsEnum a) where
  toEnum = AsEnum . toEnumDefault
  fromEnum (AsEnum x) = fromEnumDefault x

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

instance GEnum Void where
  genum = []

deriving via (AsEnum CustomEnum) instance GEnum CustomEnum
deriving via (AsEnum CustomEnum) instance Enum CustomEnum
