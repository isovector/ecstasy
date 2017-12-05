{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where

import           Control.Arrow (first, second)
import           Control.Monad.State
import           Data.Foldable (for_)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           GHC.Generics


ent :: Entity 'FieldOf
ent = def

setter :: Entity 'SetterOf
setter = def


main :: IO ()
main = do
  let init = (0, Entity def def)

  let (_, e) = flip execState init $ do
        void $ newEntity $ ent
          { pos = Just (0, 0)
          , vel = Just (1, -2)
          }

        void $ newEntity $ ent
          { pos = Just (0, 0)
          }

        let step e = do
              pos' <- pos e
              vel' <- vel e
              pure $ setter
                { pos = Just $ Just $ pos' + vel'
                }
        rmap step
        rmap step

  print $ pos e

type System w = State (Int, w 'WorldOf)

instance Num a => Num (a, a) where
  (a,b) + (c,d) = (a+c,b+d)

type V2 = (Int, Int)

data StorageType   = FieldOf | WorldOf | QueryOf | SetterOf
data ComponentType = Field   | Unique

type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: *) :: * where
  Component 'FieldOf c       a = Maybe a
  Component 'SetterOf c      a = Maybe (Maybe a)

  Component 'WorldOf 'Field  a = IntMap a
  Component 'WorldOf 'Unique a = Maybe (Int, a)

  Component 'QueryOf c       a = Maybe Bool


class Default a where
  def :: a

  -- default def :: (Generic a, GDefault a) => a
  -- def = gdef

instance Default (Maybe a) where
  def = Nothing

instance Default (IntMap a) where
  def = I.empty


data Entity f = Entity
  { pos :: Component f 'Field V2
  , vel :: Component f 'Field V2
  } deriving (Generic)

instance Default (Entity 'FieldOf) where
  def = Entity def def

instance Default (Entity 'SetterOf) where
  def = Entity def def


getEntityImpl :: Entity 'WorldOf -> Int -> Entity 'FieldOf
getEntityImpl world e =
  Entity (getComponent (pos world) e)
         (getComponent (vel world) e)


getEntity :: Int -> System Entity (Entity 'FieldOf)
getEntity e = do
  (_, es) <- get
  pure $ getEntityImpl es e

setEntity :: Int -> Entity 'SetterOf -> System Entity ()
setEntity e fs = do
  for_ (pos fs) $ \p -> modify . second $ \w -> w { pos = I.alter (const p) e $ pos w }
  for_ (vel fs) $ \p -> modify . second $ \w -> w { vel = I.alter (const p) e $ vel w }


getComponent
    :: Component 'WorldOf 'Field a
    -> Int
    -> Component 'FieldOf 'Field a
getComponent m e = I.lookup e m


fieldsToSetter
    :: Entity 'FieldOf
    -> Entity 'SetterOf
fieldsToSetter Entity{..} = Entity (Just pos) (Just vel)


nextEntity :: System a Int
nextEntity = do
  ((+1) -> e, _) <- get
  modify $ first $ const e
  pure e


newEntity :: Entity 'FieldOf -> System Entity Int
newEntity cs = do
  e <- nextEntity
  setEntity e $ fieldsToSetter cs
  pure e


rmap :: (Entity 'FieldOf -> Maybe (Entity 'SetterOf)) -> System Entity ()
rmap f = do
  (es, _) <- get
  for_ [0 .. es - 1] $ \e -> do
    cs <- getEntity e
    for_ (f cs) $ setEntity e

