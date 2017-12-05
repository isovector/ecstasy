{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Ecstasy.Types where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (Identity)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I


type SystemT w = StateT (Int, w 'WorldOf)
type System  w = SystemT w Identity


data StorageType
  = FieldOf
  | WorldOf
  | QueryOf
  | SetterOf


data ComponentType
  = Field
  | Unique


type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: *) :: * where
  Component 'FieldOf c       a = Maybe a
  Component 'SetterOf c      a = Maybe (Maybe a)

  Component 'WorldOf 'Field  a = IntMap a
  Component 'WorldOf 'Unique a = Maybe (Int, a)

  Component 'QueryOf c       a = Maybe Bool

