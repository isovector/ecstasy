{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Ecstasy
  ( module Data.Ecstasy
  , module Data.Ecstasy.Types
  , Generic
  ) where

import           Control.Arrow (first, second)
import           Control.Monad (mzero, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Trans.Reader (runReaderT, asks)
import           Control.Monad.Trans.State.Strict (modify, gets, evalStateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Ecstasy.Deriving
import qualified Data.Ecstasy.Types as T
import           Data.Ecstasy.Types hiding (unEnt)
import           Data.Foldable (for_)
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Data.Tuple (swap)
import           GHC.Generics


------------------------------------------------------------------------------
-- | This class provides all of the functionality necessary to manipulate the
-- ECS.
class HasWorld world where

  ----------------------------------------------------------------------------
  -- | Fetches an entity from the world given its 'Ent'.
  getEntity
      :: ( Monad m
         )
      => Ent
      -> SystemT world m (world 'FieldOf)
  default getEntity
      :: ( GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , Generic (world 'FieldOf)
         , Generic (world 'WorldOf)
         , Monad m
         )
      => Ent
      -> SystemT world m (world 'FieldOf)
  getEntity e = do
    w <- gets snd
    pure . to . gGetEntity (from w) $ T.unEnt e
  {-# INLINE getEntity #-}

  ----------------------------------------------------------------------------
  -- | Updates an 'Ent' in the world given its setter.
  setEntity
      :: Monad m
      => Ent
      -> world 'SetterOf
      -> SystemT world m ()
  default setEntity
      :: ( GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         , Generic (world 'WorldOf)
         , Generic (world 'SetterOf)
         , Monad m
         )
      => Ent
      -> world 'SetterOf
      -> SystemT world m ()
  setEntity e s = do
    w <- gets snd
    let x = to . gSetEntity (from s) (T.unEnt e) $ from w
    modify . second $ const x
  {-# INLINE setEntity #-}

  ----------------------------------------------------------------------------
  -- | Transforms an entity into a setter to transform the default entity into
  -- the given one. Used by 'newEntity'.
  convertSetter
      :: world 'FieldOf
      -> world 'SetterOf
  default convertSetter
      :: ( GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , Generic (world 'FieldOf)
         , Generic (world 'SetterOf)
         )
      => world 'FieldOf
      -> world 'SetterOf
  convertSetter = to . gConvertSetter . from
  {-# INLINE convertSetter #-}

  ----------------------------------------------------------------------------
  -- | The default entity, owning no components.
  defEntity :: world 'FieldOf
  default defEntity
      :: ( Generic (world 'FieldOf)
         , GDefault 'True (Rep (world 'FieldOf))
         )
      => world 'FieldOf
  defEntity = def @'True
  {-# INLINE defEntity #-}

  ----------------------------------------------------------------------------
  -- | The default setter, which keeps all components with their previous value.
  defEntity' :: world 'SetterOf
  default defEntity'
      :: ( Generic (world 'SetterOf)
         , GDefault 'True (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  defEntity' = def @'True
  {-# INLINE defEntity' #-}

  ----------------------------------------------------------------------------
  -- | A setter which will delete the entity if its 'QueryT' matches.
  delEntity :: world 'SetterOf
  default delEntity
      :: ( Generic (world 'SetterOf)
         , GDefault 'False (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  delEntity = def @'False
  {-# INLINE delEntity #-}

  ----------------------------------------------------------------------------
  -- | The default world, which contains only empty containers.
  defWorld :: world 'WorldOf
  default defWorld
      :: ( Generic (world 'WorldOf)
         , GDefault 'True (Rep (world 'WorldOf))
         )
      => world 'WorldOf
  defWorld = def @'True


instance ( Generic (world 'SetterOf)
         , Generic (world 'WorldOf)
         , Generic (world 'FieldOf)
         , GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         , GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'FieldOf))
         , GDefault 'False (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'WorldOf))
         ) => HasWorld world


------------------------------------------------------------------------------
-- | Retrieve a unique 'Ent'.
nextEntity
    :: Monad m
    => SystemT a m Ent
nextEntity = do
  (e, _) <- S.get
  modify . first . const $ e + 1
  pure $ Ent e


------------------------------------------------------------------------------
-- | Create a new entity.
newEntity
    :: (HasWorld world, Monad m)
    => world 'FieldOf
    -> SystemT world m Ent
newEntity cs = do
  e <- nextEntity
  setEntity e $ convertSetter cs
  pure e


------------------------------------------------------------------------------
-- | Delete an entity.
deleteEntity
    :: (HasWorld world, Monad m)
    => Ent
    -> SystemT world m ()
deleteEntity = flip setEntity delEntity


------------------------------------------------------------------------------
-- | Evaluate a 'QueryT'.
unQueryT
  :: QueryT world m a
  -> Ent
  -> world 'FieldOf
  -> m (Maybe a)
unQueryT q e f = runMaybeT $ runReaderT q (e, f)


------------------------------------------------------------------------------
-- | Map a 'QueryT' transformation over all entites that match it.
emap
    :: ( HasWorld world
       , Monad m
       )
    => QueryT world m (world 'SetterOf)
    -> SystemT world m ()
emap f = do
  (es, _) <- S.get
  for_ [0 .. es - 1] $ \(Ent -> e) -> do
    cs <- getEntity e
    sets <- lift $ unQueryT f e cs
    for_ sets $ setEntity e


------------------------------------------------------------------------------
-- | Collect the results of a monadic computation over every entity matching
-- a 'QueryT'.
efor
    :: ( HasWorld world
       , Monad m
       )
    => (Ent -> QueryT world m a)
    -> SystemT world m [a]
efor f = do
  (es, _) <- S.get
  fmap catMaybes $ for [0 .. es - 1] $ \(Ent -> e) -> do
    cs <- getEntity e
    lift $ unQueryT (f e) e cs


------------------------------------------------------------------------------
-- | Run a 'QueryT' over a particular 'Ent'.
runQueryT
    :: ( HasWorld world
       , Monad m
       )
    => Ent
    -> QueryT world m a
    -> SystemT world m (Maybe a)
runQueryT e qt = do
  cs <- getEntity e
  lift $ unQueryT qt e cs


------------------------------------------------------------------------------
-- | Provides a resumable 'SystemT'. This is a pretty big hack until I come up
-- with a better formalization for everything.
yieldSystemT
    :: Monad m
    => SystemState world
    -> SystemT world m a
    -> m (SystemState world, a)
yieldSystemT = (fmap swap .) . flip S.runStateT


------------------------------------------------------------------------------
-- | Evaluate a 'SystemT'.
runSystemT
    :: Monad m
    => world 'WorldOf
    -> SystemT world m a
    -> m a
runSystemT = flip evalStateT . (0,)


------------------------------------------------------------------------------
-- | Evaluate a 'System'.
runSystem
    :: world 'WorldOf
    -> System world a
    -> a
runSystem = (runIdentity .) . runSystemT


------------------------------------------------------------------------------
-- | Get the world.
getWorld
    :: Monad m
    => SystemT world m (world 'WorldOf)
getWorld = gets snd


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which have the given component.
with
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m ()
with = void . get
{-# INLINE with #-}


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which do not have the given
-- component.
without
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m ()
without f = do
  e <- asks snd
  maybe (pure ()) (const mzero) $ f e


------------------------------------------------------------------------------
-- | Get the value of a component, failing the 'QueryT' if it isn't present.
get
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m a
get f = do
  e <- asks snd
  maybe mzero pure $ f e
{-# INLINE get #-}


------------------------------------------------------------------------------
-- | Attempt to get the value of a component.
getMaybe
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m (Maybe a)
getMaybe f = fmap f $ asks snd


------------------------------------------------------------------------------
-- | Attempt to get the value of a component.
getEnt
    :: Monad m
    => QueryT world m Ent
getEnt = asks fst

