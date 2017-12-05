{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DefaultSignatures            #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE ViewPatterns                 #-}
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


-- main :: IO ()
-- main = pure ()

main :: IO ()
main = do
  let init = (0, def)

  let (_, e) = flip execState init $ do
        ex <- nextEntity
        setEntity ex $
          ent
            { pos = Just $ Just (0, 0)
            , vel = Just $ Just (1, -2)
            }

        setEntity ex $
          ent
            { pos = Nothing
            , vel = Just $ Just (4, -2)
            }

--         void $ newEntity $ ent
--           { pos = Just (0, 0)
--           }

--         let step e = do
--               pos' <- pos e
--               vel' <- vel e
--               pure $ setter
--                 { pos = Just $ Just $ pos' + vel'
--                 }
--         rmap step
--         rmap step

  print $ pos e
  print $ vel e

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


def :: forall a. (Generic a, GDefault (Rep a a)) => a
def = to $ gdef @(Rep a a)

class GDefault p where
  gdef :: p

instance GDefault (U1 a) where
  gdef = U1

instance {-# OVERLAPPING #-} GDefault (K1 i (Maybe c) p) where
  gdef = K1 Nothing

instance {-# OVERLAPPING #-} GDefault (K1 i (IntMap c) p) where
  gdef = K1 I.empty

instance GDefault c => GDefault (K1 i c p) where
  gdef = K1 gdef

instance GDefault (f p) => GDefault (M1 i c f p) where
  gdef = M1 gdef

instance (GDefault (a p), GDefault (b p)) => GDefault ((a :*: b) p) where
  gdef = gdef :*: gdef


data Entity f = Entity
  { pos :: Component f 'Field V2
  , vel :: Component f 'Field V2
  } deriving (Generic)


class GGetEntityImpl e where
  ggetEntityImpl :: e 'WorldOf -> Int -> e 'FieldOf

class (Generic (world 'SetterOf), Generic (world 'WorldOf)) => World (world :: StorageType -> *) where
  -- getEntityImpl :: world 'WorldOf -> Int -> world 'FieldOf
  -- fieldsToSetter :: world 'FieldOf -> world 'SetterOf

instance (Generic (world 'SetterOf), Generic (world 'WorldOf)) => World (world :: StorageType -> *)




-- getEntityImpl :: Entity 'WorldOf -> Int -> Entity 'FieldOf
-- getEntityImpl world e =
--   Entity (getComponent (pos world) e)
--          (getComponent (vel world) e)


-- getEntity :: World world => Int -> System world (world 'FieldOf)
-- getEntity e = do
--   (_, es) <- get
--   pure $ getEntityImpl es e


class GSetEntity a b where
  gSetEntity :: a x -> Int -> b x -> b x

instance GSetEntity (K1 i (Maybe (Maybe a))) (K1 i' (IntMap a)) where
  gSetEntity (K1 Nothing)  e (K1 b) = K1 b
  gSetEntity (K1 (Just a)) e (K1 b) = K1 $ I.alter (const a) e b

instance GSetEntity f f' => GSetEntity (M1 i c f) (M1 i' c' f') where
  gSetEntity (M1 a) e (M1 b) = M1 $ gSetEntity a e b

instance (GSetEntity a c , GSetEntity b d) => GSetEntity (a :*: b) (c :*: d) where
  gSetEntity (a :*: b) e (c :*: d) = gSetEntity a e c :*: gSetEntity b e d

type HasSetEntity world = (Generic (world 'SetterOf), Generic (world 'WorldOf), GSetEntity (Rep (world 'SetterOf)) (Rep (world 'WorldOf)))

setEntity :: HasSetEntity world => Int -> world 'SetterOf -> System world ()
setEntity e s = do
  w <- gets snd
  let x = to $ gSetEntity (from s) e $ from w
  modify $ second $ const x
  pure ()



setEntity' :: Int -> Entity 'SetterOf -> System Entity ()
setEntity' e fs = do
  for_ (pos fs) $ \p -> modify . second $ \w -> w { pos = I.alter (const p) e $ pos w }
  for_ (vel fs) $ \p -> modify . second $ \w -> w { vel = I.alter (const p) e $ vel w }


getComponent
    :: Component 'WorldOf 'Field a
    -> Int
    -> Component 'FieldOf 'Field a
getComponent m e = I.lookup e m



nextEntity :: System a Int
nextEntity = do
  ((+1) -> e, _) <- get
  modify $ first $ const e
  pure e


-- newEntity :: (HasSetEntity world, World world) => world 'FieldOf -> System world Int
-- newEntity cs = do
--   e <- nextEntity
--   setEntity e $ fieldsToSetter cs
--   pure e


-- rmap :: (HasSetEntity world, World world) => (world 'FieldOf -> Maybe (world 'SetterOf)) -> System world ()
-- rmap f = do
--   (es, _) <- get
--   for_ [0 .. es - 1] $ \e -> do
--     cs <- getEntity e
--     for_ (f cs) $ setEntity e

