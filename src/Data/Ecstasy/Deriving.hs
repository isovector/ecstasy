{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Ecstasy.Deriving where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           GHC.Generics


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


def :: (Generic a, GDefault (Rep a)) => a
def = to gdef


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

