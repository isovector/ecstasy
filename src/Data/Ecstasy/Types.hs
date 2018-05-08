{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Ecstasy.Types where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Functor.Identity (Identity)
import Data.IntMap.Strict (IntMap)


------------------------------------------------------------------------------
-- | The key for an entity.
newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e


------------------------------------------------------------------------------
-- | The internal state of the 'SystemT' monad.
type SystemState w = (Int, w 'WorldOf)

------------------------------------------------------------------------------
-- | A monad transformer over an ECS given a world 'w'.
type SystemT w = StateT (SystemState w)

------------------------------------------------------------------------------
-- | A monad over an ECS given a world 'w'.
type System  w = SystemT w Identity

------------------------------------------------------------------------------
-- | A computation to run over a particular entity.
type QueryT w m = ReaderT (Ent, w 'FieldOf) (MaybeT m)


------------------------------------------------------------------------------
-- | Data kind used to parameterize the ECS record.
data StorageType
  = FieldOf   -- ^ Used to construct the actual entity.
  | WorldOf   -- ^ Used to construct the world's storage.
  | SetterOf  -- ^ Used to construct a setter to update an entity.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | Data kind used to parameterize the fields of the ECS record.
data ComponentType
  = Field   -- ^ This component can be owned by any entity.
  | Unique  -- ^ This component can be owned by only a single entity at a time.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | Describes how we can change an 'a'.
data Update a
  = Keep   -- ^ Keep the current value.
  | Unset  -- ^ Delete the current value if it exists.
  | Set !a  -- ^ Set the current value.
  deriving (Eq, Ord, Show, Read)


------------------------------------------------------------------------------
-- | A type family to be used in your ECS recrod.
type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: *) :: * where
  Component 'FieldOf  c      a = Maybe a
  Component 'SetterOf c      a = Update a

  Component 'WorldOf 'Field  a = IntMap a
  Component 'WorldOf 'Unique a = Maybe (Int, a)

