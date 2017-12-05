{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Ecstasy
  ( module Data.Ecstasy
  , module Data.Ecstasy.Types
  , Generic
  ) where

import           Control.Arrow (first, second)
import           Control.Monad.Trans.State (get, modify, gets, evalStateT)
import           Data.Ecstasy.Deriving
import qualified Data.Ecstasy.Types as T
import           Data.Ecstasy.Types hiding (unEnt)
import           Data.Foldable (for_)
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           GHC.Generics


class World world where
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

  defEntity :: world 'FieldOf
  default defEntity
      :: ( Generic (world 'FieldOf)
         , GDefault (Rep (world 'FieldOf))
         )
      => world 'FieldOf
  defEntity = def

  defEntity' :: world 'SetterOf
  default defEntity'
      :: ( Generic (world 'SetterOf)
         , GDefault (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  defEntity' = def

  defWorld :: world 'WorldOf
  default defWorld
      :: ( Generic (world 'WorldOf)
         , GDefault (Rep (world 'WorldOf))
         )
      => world 'WorldOf
  defWorld = def


instance ( Generic (world 'SetterOf)
         , Generic (world 'WorldOf)
         , Generic (world 'FieldOf)
         , GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         , GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault (Rep (world 'FieldOf))
         , GDefault (Rep (world 'SetterOf))
         , GDefault (Rep (world 'WorldOf))
         ) => World world


nextEntity
    :: Monad m
    => SystemT a m Ent
nextEntity = do
  (e, _) <- get
  modify . first . const $ e + 1
  pure $ Ent e


newEntity
    :: (World world, Monad m)
    => world 'FieldOf
    -> SystemT world m Ent
newEntity cs = do
  e <- nextEntity
  setEntity e $ convertSetter cs
  pure e


emap
    :: ( World world
       , Monad m
       )
    => (world 'FieldOf -> Maybe (world 'SetterOf))
    -> SystemT world m ()
emap f = do
  (es, _) <- get
  for_ [0 .. es - 1] $ \(Ent -> e) -> do
    cs <- getEntity e
    for_ (f cs) $ setEntity e

efor
    :: ( World world
       , Monad m
       )
    => (Ent -> world 'FieldOf -> Maybe a)
    -> SystemT world m [a]
efor f = do
  (es, _) <- get
  fmap catMaybes $ for [0 .. es - 1] $ \(Ent -> e) -> do
    cs <- getEntity e
    pure $ f e cs


runSystemT
    :: Monad m
    => world 'WorldOf
    -> SystemT world m a
    -> m a
runSystemT = flip evalStateT . (0,)


getWorld
    :: Monad m
    => SystemT world m (world 'WorldOf)
getWorld = gets snd
