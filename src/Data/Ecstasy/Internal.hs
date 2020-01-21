{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Data.Ecstasy.Internal where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Control.Monad.Codensity (lowerCodensity)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import qualified Control.Monad.Trans.Reader as R
import           Data.Coerce
import           Data.Ecstasy.Internal.Deriving
import qualified Data.Ecstasy.Types as T
import           Data.Ecstasy.Types hiding (unEnt)
import           Data.Foldable (for_)
import           Data.IORef
import qualified Data.IntMap as IM
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           GHC.Generics
import           Lens.Micro ((.~))


------------------------------------------------------------------------------
-- | This class provides all of the functionality necessary to manipulate the
-- ECS.
class (Monad m, MonadIO m, HasWorld' world) => HasWorld world m where

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
    w <- getWorld
    lift . lowerCodensity
         . fmap to
         . gGetEntity @m (from w)
         $ T.unEnt e
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
    w <- getWorld
    x <- lift . lowerCodensity
              . fmap to
              . gSetEntity (from s) (T.unEnt e)
              $ from w
    modify $ ssWorld .~ x
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
  {-# INLINE defStorage #-}


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
         , MonadIO m
         ) => HasWorld world m


------------------------------------------------------------------------------
-- | Utilities for defining 'surgery'.
class StorageSurgeon t m world where
  ----------------------------------------------------------------------------
  -- | Hoist storage through a monad transformer.
  hoistStorage
    :: world ('WorldOf m)
    -> world ('WorldOf (t m))
  default hoistStorage
    :: ( Generic (world ('WorldOf m))
       , Generic (world ('WorldOf (t m)))
       , GHoistWorld t m
                     (Rep (world ('WorldOf m)))
                     (Rep (world ('WorldOf (t m))))
       )
    => world ('WorldOf m)
    -> world ('WorldOf (t m))
  hoistStorage = to . gHoistWorld @t @m . from
  {-# INLINE hoistStorage #-}

  ----------------------------------------------------------------------------
  -- | Grafts two worlds together, using data from the second argument and
  -- vtables from the first.
  graftStorage
    :: world ('WorldOf m)
    -> world ('WorldOf (t m))
    -> world ('WorldOf m)
  default graftStorage
    :: ( Generic (world ('WorldOf m))
       , Generic (world ('WorldOf (t m)))
       , GGraft (Rep (world ('WorldOf m)))
                (Rep (world ('WorldOf (t m))))
       )
    => world ('WorldOf m)
    -> world ('WorldOf (t m))
    -> world ('WorldOf m)
  graftStorage a b = to $ gGraft (from a) (from b)
  {-# INLINE graftStorage #-}


instance ( Generic (world ('WorldOf m))
         , Generic (world ('WorldOf (t m)))
         , GHoistWorld t m (Rep (world ('WorldOf m)))
                           (Rep (world ('WorldOf (t m))))
         , GGraft (Rep (world ('WorldOf m)))
                  (Rep (world ('WorldOf (t m))))
         , MonadTrans t
         ) => StorageSurgeon t m world


------------------------------------------------------------------------------
-- | Run a monad transformer /underneath/ a 'SystemT'.
--
-- Due to the recursive interactions between 'SystemT' and 'QueryT', we're
-- often unable to put a temporary monad transformer on the top of the stack.
-- As a result, often 'surgery' is our ony means of introducting ephemeral
-- effects.
--
-- @
-- draw :: 'SystemT' World IO [Graphics]
-- draw = fmap fst . 'surgery' runWriterT $
--   for_ thingsToRender $ \\thingy ->
--     tell [thingy]
-- @
--
-- Note: Any hooks installed will *not* be run under surgery.
surgery
    :: ( Monad (t m)
       , MonadIO (t m)
       , Monad m
       , MonadIO m
       , StorageSurgeon t m world
       )
    => (forall x. t m x -> m (x, b))
    -> SystemT world (t m) a
    -> SystemT world m (b, a)
surgery f m = SystemT $ R.ReaderT $ \ref -> do
  SystemState i s h <- liftIO $ readIORef ref
  ((SystemState i' s' _, a), b) <-
    f $ yieldSystemT (SystemState i (hoistStorage s) defHooks) m
  liftIO . writeIORef ref $ SystemState i' (graftStorage s s') h
  pure (b, a)


------------------------------------------------------------------------------
-- | Retrieve a unique 'Ent'.
nextEntity
    :: (Monad m, MonadIO m)
    => SystemT a m Ent
nextEntity = do
  e <- gets _ssNextId
  modify $ ssNextId .~ e + 1
  pure $ Ent e


------------------------------------------------------------------------------
-- | Create a new entity.
createEntity
    :: (HasWorld world m, Monad m)
    => world 'FieldOf
    -> SystemT world m Ent
createEntity cs = do
  h <- gets _ssHooks
  e <- nextEntity
  setEntity e $ convertSetter cs
  hookNewEnt h e
  pure e


------------------------------------------------------------------------------
-- | Delete an entity.
deleteEntity
   :: (HasWorld world m, Monad m)
    => Ent
    -> SystemT world m ()
deleteEntity e = do
  h <- gets _ssHooks
  hookDelEnt h e
  setEntity e delEntity


------------------------------------------------------------------------------
-- | Evaluate a 'QueryT'.
unQueryT
  :: QueryT world m a
  -> Ent
  -> SystemState world m
  -> m (Maybe a)
unQueryT q e f = runMaybeT $ flip R.runReaderT (e, f) $ runQueryT' q


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
  world <- gets id
  let es = t world
  for_ es $ \e -> do
    cs <- gets id
    sets <- lift $ unQueryT f e cs
    for_ sets $ setEntity e


------------------------------------------------------------------------------
-- | Collect the results of a monadic computation over every entity matching
-- a 'QueryT'.
efor
    :: ( HasWorld world m
       , Monad m
       , MonadIO m
       )
    => EntTarget world m
    -> QueryT world m a
    -> SystemT world m [a]
efor t f = do
  world <- gets id
  let es = t world
  fmap catMaybes $ for es $ \e -> do
    cs <- gets id
    lift $ unQueryT f e cs


------------------------------------------------------------------------------
-- | Do an 'emap' and an 'efor' at the same time.
eover
    :: ( HasWorld world m
       , MonadIO m
       , Monad m
       )
    => EntTarget world m
    -> QueryT world m (a, world 'SetterOf)
    -> SystemT world m [a]
eover t f = do
  world <- gets id
  let es = t world
  fmap catMaybes $ for es $ \e -> do
    cs <- gets id
    mset <- lift $ unQueryT f e cs
    for mset $ \(a, setter) -> do
      setEntity e setter
      pure a


------------------------------------------------------------------------------
-- | Run a 'QueryT' over a particular 'Ent'.
runQueryT
    :: ( HasWorld world m
       , Monad m
       , MonadIO m
       )
    => Ent
    -> QueryT world m a
    -> SystemT world m (Maybe a)
runQueryT e qt = do
  cs <- gets id
  lift $ unQueryT qt e cs


getWorld :: (MonadIO m, Monad m) => SystemT world m (world ('WorldOf m))
getWorld = gets _ssWorld


------------------------------------------------------------------------------
-- | Provides a resumable 'SystemT'. This is a pretty big hack until I come up
-- with a better formalization for everything.
yieldSystemT
    :: (MonadIO m, Monad m)
    => SystemState world m
    -> SystemT world m a
    -> m (SystemState world m, a)
yieldSystemT ss m = do
  ref <- liftIO $ newIORef ss
  a <- R.runReaderT (runSystemT' m) ref
  ss' <- liftIO $ readIORef ref
  pure (ss', a)


------------------------------------------------------------------------------
-- | Evaluate a 'SystemT'.
runSystemT
    :: (Monad m, MonadIO m)
    => world ('WorldOf m)
    -> SystemT world m a
    -> m a
runSystemT w m = do
  ref <- liftIO . newIORef $ SystemState 0 w defHooks
  R.runReaderT (runSystemT' m) ref


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which have the given component.
with
    :: forall m c a world
     . ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m ()
with = void . query @c @a
{-# INLINE with #-}


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which do not have the given
-- component.
without
    :: forall m c a world
     . ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m ()
without f = queryMaybe @c @a f >>= maybe (pure ()) (const empty)


------------------------------------------------------------------------------
-- | Get the value of a component, failing the 'QueryT' if it isn't present.
query
    :: forall c a m world
     . ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m a
query f = queryMaybe f >>= maybe empty pure


queryTarget
    :: Monad m
    => EntTarget world m
    -> QueryT world m [Ent]
queryTarget t = do
  (_, w) <- QueryT R.ask
  pure $ t w

------------------------------------------------------------------------------
-- | Run a subquery inside of a 'QueryT'.
subquery
    :: ( HasWorld world m
       , Monad m
       , MonadIO m
       )
    => EntTarget world m
    -> QueryT world m a
    -> QueryT world m [a]
subquery t q = do
  (_, w) <- QueryT R.ask
  let es = t w
  fmap catMaybes $ for es $ \e -> do
    lift $ unQueryT q e w


------------------------------------------------------------------------------
-- | Attempt to get the value of a component.
queryMaybe
    :: forall c a m world
     . ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m (Maybe a)
queryMaybe f = do
  (Ent e, w) <- QueryT R.ask
  lift $ getField @c (f $ _ssWorld w) e


------------------------------------------------------------------------------
-- | Attempt to get the owner and value of a unique component.
queryUnique
    :: ( Monad m
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) 'Unique a)
    -> QueryT world m (Maybe (Ent, a))
queryUnique f = do
  (_, w) <- QueryT R.ask
  pure $ coerce $ f $ _ssWorld w


------------------------------------------------------------------------------
-- | Get the 'Ent' for whom this query is running.
queryEnt
    :: Monad m
    => QueryT world m Ent
queryEnt = QueryT $ R.asks fst


------------------------------------------------------------------------------
-- | Get the 'Ent' for whom this query is running.
querySelf
    :: (MonadIO m, HasWorld world m)
    => QueryT world m (world 'FieldOf)
querySelf = QueryT $ R.ReaderT $ \(e, w) ->
  MaybeT $ fmap Just $ runSystemT (_ssWorld w) $ getEntity e


------------------------------------------------------------------------------
-- | Query a flag as a 'Bool'.
queryFlag
    :: forall m c a world
     . ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m Bool
queryFlag = fmap (maybe False (const True)) . queryMaybe @c @a


------------------------------------------------------------------------------
-- | Perform a query with a default.
queryDef
    :: forall m c a world. ( Monad m
       , GetField c
       , IsInjective m c a
       )
    => a
    -> (world ('WorldOf m) -> Component ('WorldOf m) c a)
    -> QueryT world m a
queryDef z = fmap (maybe z id) . queryMaybe @c


------------------------------------------------------------------------------
-- | An 'EntTarget' is a set of 'Ent's to iterate over.
type EntTarget world m = SystemState world m -> [Ent]


------------------------------------------------------------------------------
-- | Lifted 'gets' for 'SystemT'.
gets
    :: (Monad m, MonadIO m)
    => (SystemState world m -> a)
    -> SystemT world m a
gets f = do
  ref <- SystemT R.ask
  ss <- liftIO $ readIORef ref
  pure $ f ss


------------------------------------------------------------------------------
-- | Lifted 'modify' for 'SystemT'.
modify
    :: (Monad m, MonadIO m)
    => (SystemState world m -> SystemState world m)
    -> SystemT world m ()
modify f = do
  ref <- SystemT R.ask
  liftIO $ modifyIORef ref f


------------------------------------------------------------------------------
-- | Iterate over all entities.
allEnts :: (MonadIO m, Monad m) => EntTarget world m
allEnts world = coerce [0 .. _ssNextId world - 1]


entsWith
  :: (MonadIO m, Monad m)
  => (world ('WorldOf m) -> Component ('WorldOf m) 'Field a)
  -> EntTarget world m
entsWith f = coerce . IM.keys . f . _ssWorld


------------------------------------------------------------------------------
-- | Target the entity uniquely identified by owning a 'Unique' field.
uniqueEnt
  :: ( Monad m
     , MonadIO m
     )
  => (world ('WorldOf m) -> Component ('WorldOf m) 'Unique a)
  -> EntTarget world m
uniqueEnt f = maybe [] (pure . coerce . fst) . f . _ssWorld


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

