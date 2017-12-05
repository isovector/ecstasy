{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Ecstasy.Types where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (Identity)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I


newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e



type SystemT w = StateT (Int, w 'WorldOf)
type System  w = SystemT w Identity


data StorageType
  = FieldOf
  | WorldOf
  | SetterOf


data ComponentType
  = Field
  | Unique


data Update a
  = Keep
  | Unset
  | Set a


type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: *) :: * where
  Component 'FieldOf  c      a = Maybe a
  Component 'SetterOf c      a = Update a

  Component 'WorldOf 'Field  a = IntMap a
  Component 'WorldOf 'Unique a = Maybe (Int, a)

