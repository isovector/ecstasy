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
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE ViewPatterns                 #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where

import Data.Functor.Identity (Identity)
import Debug.Trace
import           Control.Arrow (first, second)
import           Control.Monad.Trans.State
import           Control.Monad (void)
import           Data.Foldable (for_)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           GHC.Generics


-- main :: IO ()
-- main = pure ()

main :: IO ()
main = do
  let init = (0, defWorld)

  (_, e) <-  flip execStateT init $ do
        void $ newEntity $ defEntity
            { pos = Just (0, 0)
            , vel = Just (1, -2)
            , ack = Just True
            }

        void $ newEntity $ defEntity
          { pos = Just (0, 0)
          , ack = Just False
          }

        let
          step e = do
            pos' <- pos e
            vel' <- vel e
            pure $ defEntity'
              { pos = Just $ Just $ pos' + vel'
              }
        rmap step
        rmap step

  print $ pos e
  print $ vel e
  print $ ack e

type SystemT w = StateT (Int, w 'WorldOf)
type System w = SystemT w Identity

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


def :: forall a. (Generic a, GDefault (Rep a)) => a
def = to $ gdef @(Rep a)

class GDefault f where
  gdef :: f a

instance GDefault U1 where
  gdef = U1

instance GDefault (K1 i (Maybe c)) where
  gdef = K1 Nothing

instance GDefault (K1 i (IntMap c)) where
  gdef = K1 I.empty

instance GDefault f => GDefault (M1 i c f) where
  gdef = M1 gdef

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gdef = gdef :*: gdef


data Entity f = Entity
  { pos :: Component f 'Field V2
  , vel :: Component f 'Field V2
  , ack :: Component f 'Unique Bool
  } deriving (Generic)


class World world where
  getEntity
      :: ( World world
         , Monad m
         )
      => Int
      -> SystemT world m (world 'FieldOf)
  default getEntity
      :: ( GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , Generic (world 'FieldOf)
         , Generic (world 'WorldOf)
         , Monad m
         )
      => Int
      -> SystemT world m (world 'FieldOf)
  getEntity e = do
    w <- gets snd
    pure $ to $ gGetEntity (from w) e

  setEntity
      :: Monad m
      => Int
      -> world 'SetterOf
      -> SystemT world m ()
  default setEntity
      :: ( GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         , Generic (world 'WorldOf)
         , Generic (world 'SetterOf)
         , Monad m
         )
      => Int
      -> world 'SetterOf
      -> SystemT world m ()
  setEntity e s = do
    w <- gets snd
    let x = to $ gSetEntity (from s) e $ from w
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




class GConvertSetter a b where
  gConvertSetter :: a x -> b x

instance GConvertSetter (K1 i a) (K1 i' (Maybe a)) where
  gConvertSetter (K1 a) = K1 $ Just a

instance GConvertSetter f f' => GConvertSetter (M1 i c f) (M1 i' c' f') where
  gConvertSetter (M1 a) = M1 $ gConvertSetter a

instance (GConvertSetter a c , GConvertSetter b d) => GConvertSetter (a :*: b) (c :*: d) where
  gConvertSetter (a :*: b) = gConvertSetter a :*: gConvertSetter b


class GGetEntity a b where
  gGetEntity :: a x -> Int -> b x

instance GGetEntity (K1 i (IntMap a)) (K1 i' (Maybe a)) where
  gGetEntity (K1 a) e = K1 $ I.lookup e $ a

instance GGetEntity (K1 i (Maybe (Int, a))) (K1 i' (Maybe a)) where
  gGetEntity (K1 (Just (e', a))) e | e == e' = K1 $ Just a
  gGetEntity _ _ = K1 Nothing

instance GGetEntity f f' => GGetEntity (M1 i c f) (M1 i' c' f') where
  gGetEntity (M1 a) e = M1 $ gGetEntity a e

instance (GGetEntity a c , GGetEntity b d) => GGetEntity (a :*: b) (c :*: d) where
  gGetEntity (a :*: b) e = gGetEntity a e :*: gGetEntity b e


class GSetEntity a b where
  gSetEntity :: a x -> Int -> b x -> b x

instance GSetEntity (K1 i (Maybe (Maybe a))) (K1 i' (Maybe (Int, a))) where
  gSetEntity (K1 (Just (Just a))) e _ = K1 $ Just (e, a)
  gSetEntity (K1 (Just Nothing)) e (K1 (Just (e', b))) =
    if e == e'
       then K1 Nothing
       else K1 $ Just (e', b)
  gSetEntity _  _ (K1 b) = K1 b

instance GSetEntity (K1 i (Maybe (Maybe a))) (K1 i' (IntMap a)) where
  gSetEntity (K1 Nothing)  e (K1 b) = K1 b
  gSetEntity (K1 (Just a)) e (K1 b) = K1 $ I.alter (const a) e b

instance GSetEntity f f' => GSetEntity (M1 i c f) (M1 i' c' f') where
  gSetEntity (M1 a) e (M1 b) = M1 $ gSetEntity a e b

instance (GSetEntity a c , GSetEntity b d) => GSetEntity (a :*: b) (c :*: d) where
  gSetEntity (a :*: b) e (c :*: d) = gSetEntity a e c :*: gSetEntity b e d


nextEntity :: Monad m => SystemT a m Int
nextEntity = do
  (e, _) <- get
  modify . first . const $ e + 1
  pure e


newEntity :: (World world, Monad m) => world 'FieldOf -> SystemT world m Int
newEntity cs = do
  e <- nextEntity
  setEntity e $ convertSetter cs
  pure e



rmap :: (World world, Monad m) => (world 'FieldOf -> Maybe (world 'SetterOf)) -> SystemT world m ()
rmap f = do
  (es, _) <- get
  for_ [0 .. es - 1] $ \e -> do
    cs <- getEntity e
    for_ (f cs) $ setEntity e

