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

module Record where

import GHC.Generics (Generic)
import qualified GHC.Generics as G
import GHC.TypeLits

import Data.Proxy
import Data.Void

--------------------------------------------------------------------------------

type family (:::) (tag :: k) (label :: Symbol) :: *
infix 5 :::

type instance (:::) Void label = Void

data Shown

type instance (:::) Shown label = Proxy label

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

instance Enum CustomEnum where
  toEnum 0 = First undefined
  toEnum 1 = Second undefined
  toEnum _ = error "Bad argument"
  fromEnum (First _) = 0
  fromEnum (Second _) = 1

