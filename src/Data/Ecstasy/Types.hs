{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields   #-}

module Data.Ecstasy.Types where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer.Class (MonadWriter)
import Data.Data
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.Kind
import GHC.Generics (Generic)
import Lens.Micro


------------------------------------------------------------------------------
-- | The key for an entity.
newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord, Data, Typeable)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e


------------------------------------------------------------------------------
-- | The internal state of the 'SystemT' monad.
data SystemState w m = SystemState
  { _ssNextId :: {-# UNPACK #-} !Int
  , _ssWorld  :: w ('WorldOf m)
  , _ssHooks  :: {-# UNPACK #-} !(Hooks w m)
  } deriving (Generic)

ssNextId :: Lens' (SystemState w m) Int
ssNextId f (SystemState a b c) = (\a' -> SystemState a' b c) <$> f a

ssWorld :: Lens' (SystemState w m) (w ('WorldOf m))
ssWorld f (SystemState a b c) = (\b' -> SystemState a b' c) <$> f b

ssHooks :: Lens' (SystemState w m) (Hooks w m)
ssHooks f (SystemState a b c) = (\c' -> SystemState a b c') <$> f c


------------------------------------------------------------------------------
-- | A datastructure holding hooks into ecstasy's entity management.
data Hooks w m = Hooks
  { hookNewEnt :: Ent -> SystemT w m ()
  , hookDelEnt :: Ent -> SystemT w m ()
  } deriving (Generic)

defHooks :: Monad m => Hooks w m
defHooks = Hooks (const $ pure ()) (const $ pure ())


------------------------------------------------------------------------------
-- | A monad transformer over an ECS given a world 'w'.
newtype SystemT w m a = SystemT
  { runSystemT' :: ReaderT (IORef (SystemState w m)) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadWriter ww
           , MonadIO
           )

instance MonadTrans (SystemT w) where
  lift = SystemT . lift

instance MonadReader r m => MonadReader r (SystemT w m) where
  ask = SystemT $ lift ask
  local z s = SystemT . ReaderT $ \r ->
    local z $ runReaderT (runSystemT' s) r


------------------------------------------------------------------------------
-- | A computation to run over a particular entity.
newtype QueryT w m a = QueryT
  { runQueryT' :: ReaderT (Ent, SystemState w m) (MaybeT m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadWriter ww
           , MonadIO
           , Alternative
           , MonadPlus
           , MonadFail
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
  | Modify !(a -> a)


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


------------------------------------------------------------------------------
-- | The inverse of 'Component ('WorldOf m)' -- used to prove 'IsInjective'
type family Inverse (m :: Type -> Type)
                    (r :: Type) :: ComponentType where
  Inverse m (IntMap a)       = 'Field
  Inverse m (Maybe (Int, a)) = 'Unique
  Inverse m (VTable m a)     = 'Virtual

------------------------------------------------------------------------------
-- | A proof that 'c' is injective.
class (c ~ Inverse m (Component ('WorldOf m) c a))
    => IsInjective m (c :: ComponentType) a
instance (c ~ Inverse m (Component ('WorldOf m) c a))
    => IsInjective m (c :: ComponentType) a

