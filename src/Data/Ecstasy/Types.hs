{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields   #-}

module Data.Ecstasy.Types where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Control.Monad.Writer.Class (MonadWriter)
import Data.Functor.Identity (Identity)
import Data.IntMap.Strict (IntMap)
import Data.Kind


------------------------------------------------------------------------------
-- | The key for an entity.
newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e


------------------------------------------------------------------------------
-- | The internal state of the 'SystemT' monad.
type SystemState w m = (Int, w ('WorldOf m))

------------------------------------------------------------------------------
-- | A monad transformer over an ECS given a world 'w'.
newtype SystemT w m a = SystemT
  { runSystemT' :: StateT (SystemState w m) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader r
           , MonadWriter ww
           , MonadIO
           )

instance MonadTrans (SystemT w) where
  lift = SystemT . lift

instance MonadState s m => MonadState s (SystemT w m) where
  get = SystemT . lift $ get
  put = SystemT . lift . put


------------------------------------------------------------------------------
-- | A monad over an ECS given a world 'w'.
type System w = SystemT w Identity


------------------------------------------------------------------------------
-- | A computation to run over a particular entity.
newtype QueryT w m a = QueryT
  { runQueryT' :: ReaderT (Ent, w 'FieldOf) (MaybeT m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadWriter ww
           , MonadIO
           , Alternative
           , MonadPlus
           )

instance MonadTrans (QueryT w) where
  lift = QueryT . lift . lift

instance MonadReader r m => MonadReader r (QueryT w m) where
  ask = QueryT $ lift ask
  local f = QueryT . runQueryT' . local f


------------------------------------------------------------------------------
-- | A collection of methods necessary to dispatch reads and writes to
-- a 'Virtual' component.
data VTable m a = VTable
  { -- | Get the value of an entity's component.
    vget :: !(Ent -> m (Maybe a))

    -- | Update the value of an entity's component.
  , vset :: !(Ent -> Update a -> m ())
  }


------------------------------------------------------------------------------
-- | Data kind used to parameterize the ECS record.
data StorageType
  = FieldOf   -- ^ Used to describe the actual entity.
  | WorldOf (Type -> Type)  -- ^ Used to construct the world's storage.
  | SetterOf  -- ^ Used to construct a setter to update an entity.


------------------------------------------------------------------------------
-- | Data kind used to parameterize the fields of the ECS record.
data ComponentType
  = Field      -- ^ This component can be owned by any entity.
  | Unique     -- ^ This component can be owned by only a single entity at a time.
  | Virtual    -- ^ This component is owned by another system.


------------------------------------------------------------------------------
-- | Describes how we can change an 'a'.
data Update a
  = Keep   -- ^ Keep the current value.
  | Unset  -- ^ Delete the current value if it exists.
  | Set !a  -- ^ Set the current value.
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)


------------------------------------------------------------------------------
-- | A type family to be used in your ECS recrod.
type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: Type) :: Type where
  Component 'FieldOf  c      a = Maybe a
  Component 'SetterOf c      a = Update a

  Component ('WorldOf m) 'Field   a = IntMap a
  Component ('WorldOf m) 'Unique  a = Maybe (Int, a)
  Component ('WorldOf m) 'Virtual a = VTable m a

