{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Record where

import GHC.Generics (Generic)
import qualified GHC.Generics as G

data CustomEnum
  = First
  | Second
  deriving (Generic)

