{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Data.Ecstasy.Internal where

import           Control.Arrow (first, second)
import           Control.Monad (mzero, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Trans.Reader (runReaderT, asks)
import           Control.Monad.Trans.State.Strict (modify, get, gets, evalStateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Ecstasy.Deriving
import qualified Data.Ecstasy.Types as T
import           Data.Ecstasy.Types hiding (unEnt)
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Data.Tuple (swap)
import           GHC.Generics


------------------------------------------------------------------------------
-- | This class provides all of the functionality necessary to manipulate the
-- ECS.
class HasWorld' world => HasWorld world m where

  ----------------------------------------------------------------------------
  -- | Fetches an entity from the world given its 'Ent'.
  getEntity
      :: Monad m
      => Ent
      -> SystemT world m (world 'FieldOf)
  default getEntity
      :: ( Monad m
         , GGetEntity m
                      (Rep (world ('WorldOf m)))
                      (Rep (world 'FieldOf))
         , Generic (world 'FieldOf)
         , Generic (world ('WorldOf m))
         )
      => Ent
      -> SystemT world m (world 'FieldOf)
  getEntity e = do
    w <- SystemT $ gets snd
    lift . fmap to . gGetEntity @m (from w) $ T.unEnt e
  {-# INLINE getEntity #-}

  ----------------------------------------------------------------------------
  -- | Updates an 'Ent' in the world given its setter.
  setEntity
      :: Ent
      -> world 'SetterOf
      -> SystemT world m ()
  default setEntity
      :: ( GSetEntity m
                      (Rep (world 'SetterOf))
                      (Rep (world ('WorldOf m)))
         , Generic (world ('WorldOf m))
         , Generic (world 'SetterOf)
         , Monad m
         )
      => Ent
      -> world 'SetterOf
      -> SystemT world m ()
  setEntity e s = do
    w <- SystemT $ gets snd
    x <- lift . fmap to . gSetEntity (from s) (T.unEnt e) $ from w
    SystemT . modify . second $ const x
  {-# INLINE setEntity #-}

  ----------------------------------------------------------------------------
  -- | The default world, which contains only empty containers.
  defStorage :: world ('WorldOf m)
  default defStorage
      :: ( Generic (world ('WorldOf m))
         , GDefault 'True (Rep (world ('WorldOf m)))
         )
      => world ('WorldOf m)
  defStorage = def @'True


class HasWorld' world where
  ----------------------------------------------------------------------------
  -- | Transforms an entity into a setter to transform the default entity into
  -- the given one. Used by 'createEntity'.
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
  newEntity :: world 'FieldOf
  default newEntity
      :: ( Generic (world 'FieldOf)
         , GDefault 'True (Rep (world 'FieldOf))
         )
      => world 'FieldOf
  newEntity = def @'True
  {-# INLINE newEntity #-}

  ----------------------------------------------------------------------------
  -- | The default setter, which keeps all components with their previous value.
  unchanged :: world 'SetterOf
  default unchanged
      :: ( Generic (world 'SetterOf)
         , GDefault 'True (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  unchanged = def @'True
  {-# INLINE unchanged #-}

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


instance ( Generic (world 'SetterOf)
         , Generic (world 'FieldOf)
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'FieldOf))
         , GDefault 'False (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'SetterOf))
         ) => HasWorld' world


instance ( HasWorld' world
         , Generic (world 'SetterOf)
         , Generic (world ('WorldOf m))
         , Generic (world 'FieldOf)
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'FieldOf))
         , GDefault 'False (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world ('WorldOf m)))
         , GSetEntity m
                      (Rep (world 'SetterOf))
                      (Rep (world ('WorldOf m)))
         , GGetEntity m
                      (Rep (world ('WorldOf m)))
                      (Rep (world 'FieldOf))
         , Monad m
         ) => HasWorld world m


------------------------------------------------------------------------------
-- | Retrieve a unique 'Ent'.
nextEntity
    :: Monad m
    => SystemT a m Ent
nextEntity = do
  (e, _) <- SystemT S.get
  SystemT . modify . first . const $ e + 1
  pure $ Ent e


------------------------------------------------------------------------------
-- | Create a new entity.
createEntity
    :: (HasWorld world m, Monad m)
    => world 'FieldOf
    -> SystemT world m Ent
createEntity cs = do
  e <- nextEntity
  setEntity e $ convertSetter cs
  pure e


------------------------------------------------------------------------------
-- | Delete an entity.
deleteEntity
    :: (HasWorld world m, Monad m)
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
unQueryT q e f = runMaybeT $ flip runReaderT (e, f) $ runQueryT' q


------------------------------------------------------------------------------
-- | Map a 'QueryT' transformation over all entites that match it.
emap
    :: ( HasWorld world m
       , Monad m
       )
    => EntTarget world m
    -> QueryT world m (world 'SetterOf)
    -> SystemT world m ()
emap t f = do
  es <- t
  for_ es $ \e -> do
    cs <- getEntity e
    sets <- lift $ unQueryT f e cs
    for_ sets $ setEntity e


------------------------------------------------------------------------------
-- | Collect the results of a monadic computation over every entity matching
-- a 'QueryT'.
efor
    :: ( HasWorld world m
       , Monad m
       )
    => EntTarget world m
    -> QueryT world m a
    -> SystemT world m [a]
efor t f = do
  es <- t
  fmap catMaybes $ for es $ \e -> do
    cs <- getEntity e
    lift $ unQueryT f e cs


------------------------------------------------------------------------------
-- | Do an 'emap' and an 'efor' at the same time.
eover
    :: ( HasWorld world m
       , Monad m
       )
    => EntTarget world m
    -> QueryT world m (a, world 'SetterOf)
    -> SystemT world m [a]
eover t f = do
  es <- t
  fmap catMaybes $ for es $ \e -> do
    cs <- getEntity e
    mset <- lift $ unQueryT f e cs
    for mset $ \(a, setter) -> do
      setEntity e setter
      pure a


------------------------------------------------------------------------------
-- | Run a 'QueryT' over a particular 'Ent'.
runQueryT
    :: ( HasWorld world m
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
    => SystemState world m
    -> SystemT world m a
    -> m (SystemState world m, a)
yieldSystemT w = fmap swap . flip S.runStateT w . runSystemT'


------------------------------------------------------------------------------
-- | Evaluate a 'SystemT'.
runSystemT
    :: Monad m
    => world ('WorldOf m)
    -> SystemT world m a
    -> m a
runSystemT w = flip evalStateT (0, w) . runSystemT'


------------------------------------------------------------------------------
-- | Evaluate a 'System'.
runSystem
    :: world ('WorldOf Identity)
    -> System world a
    -> a
runSystem = (runIdentity .) . runSystemT


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which have the given component.
with
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m ()
with = void . query
{-# INLINE with #-}


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which do not have the given
-- component.
without
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m ()
without f = do
  e <- QueryT $ asks snd
  maybe (pure ()) (const mzero) $ f e


------------------------------------------------------------------------------
-- | Get the value of a component, failing the 'QueryT' if it isn't present.
query
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m a
query f = do
  e <- QueryT $ asks snd
  maybe mzero pure $ f e
{-# INLINE query #-}


------------------------------------------------------------------------------
-- | Attempt to get the value of a component.
queryMaybe
    :: Monad m
    => (world 'FieldOf -> Maybe a)
    -> QueryT world m (Maybe a)
queryMaybe f = fmap f $ QueryT $ asks snd


------------------------------------------------------------------------------
-- | Get the 'Ent' for whom this query is running.
queryEnt
    :: Monad m
    => QueryT world m Ent
queryEnt = QueryT $ asks fst


------------------------------------------------------------------------------
-- | Query a flag as a 'Bool'.
queryFlag
    :: Monad m
    => (world 'FieldOf -> Maybe ())
    -> QueryT world m Bool
queryFlag = fmap (maybe False (const True)) . queryMaybe


------------------------------------------------------------------------------
-- | Perform a query with a default.
queryDef
    :: Monad m
    => z
    -> (world 'FieldOf -> Maybe z)
    -> QueryT world m z
queryDef z = fmap (maybe z id) . queryMaybe


------------------------------------------------------------------------------
-- | An 'EntTarget' is a set of 'Ent's to iterate over.
type EntTarget world m = SystemT world m [Ent]


------------------------------------------------------------------------------
-- | Iterate over all entities.
allEnts :: Monad m => EntTarget world m
allEnts = do
  (es, _) <- SystemT get
  pure $ Ent <$> [0 .. es - 1]


------------------------------------------------------------------------------
-- | Iterate over some entities.
someEnts :: Monad m => [Ent] -> EntTarget world m
someEnts = pure


------------------------------------------------------------------------------
-- | Iterate over an entity.
anEnt :: Monad m => Ent -> EntTarget world m
anEnt = pure . pure


------------------------------------------------------------------------------
-- | Turn a 'Maybe' into an 'Update'.
maybeToUpdate :: Maybe a -> Update a
maybeToUpdate Nothing  = Unset
maybeToUpdate (Just a) = Set a

