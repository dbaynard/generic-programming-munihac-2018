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

import Data.Proxy
import Data.Void

--------------------------------------------------------------------------------

type family (:::) (tag :: k) (label :: Symbol) :: *
infix 5 :::

type instance (:::) Void label = Void
type instance (:::) () label = ()

data Shown

type instance (:::) Shown label = Proxy label

--------------------------------------------------------------------------------

newtype Default a = Default a

instance (Generic a, GEq' (G.Rep a)) => Eq (Default a) where
  Default x == Default y = x `geqdefault` y

instance (Generic a, GEq' (G.Rep a)) => GEq (Default a) where
  Default x `geq` Default y = x `geqdefault` y

instance (Generic a, GEq a, Enum' (G.Rep a)) => Enum (Default a) where
  toEnum = Default . toEnumDefault
  fromEnum (Default x) = fromEnumDefault x

instance (Generic a, GEq a, Enum' (G.Rep a)) => GEnum (Default a) where
  genum = Default . G.to <$> enum'

--------------------------------------------------------------------------------

data CustomEnum_ w
  = First (w ::: "The first")
  | Second (w ::: "The second")
  deriving stock (Generic)

type CustomEnum = CustomEnum_ ()

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

deriving via (Default CustomEnum) instance Eq CustomEnum
deriving via (Default CustomEnum) instance GEq CustomEnum

deriving via (Default CustomEnum) instance GEnum CustomEnum
deriving via (Default CustomEnum) instance Enum CustomEnum
