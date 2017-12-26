{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Ecstasy.Types where

import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State (StateT)
import           Data.Functor.Identity (Identity)
import           Data.IntMap (IntMap)


newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e



type SystemT w = StateT (Int, w 'WorldOf)
type System  w = SystemT w Identity

type QueryT w m = ReaderT (w 'FieldOf) (MaybeT m)


data StorageType
  = FieldOf
  | WorldOf
  | SetterOf
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data ComponentType
  = Field
  | Unique
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Update a
  = Keep
  | Unset
  | Set a
  deriving (Eq, Ord, Show, Read)


type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: *) :: * where
  Component 'FieldOf  c      a = Maybe a
  Component 'SetterOf c      a = Update a

  Component 'WorldOf 'Field  a = IntMap a
  Component 'WorldOf 'Unique a = Maybe (Int, a)

