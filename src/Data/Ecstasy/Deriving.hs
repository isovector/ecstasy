{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Ecstasy.Deriving where

import           Data.Ecstasy.Types (Update (..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           GHC.Generics


class GConvertSetter a b where
  gConvertSetter :: a x -> b x

instance GConvertSetter (K1 i a) (K1 i' (Maybe a)) where
  gConvertSetter (K1 a) = K1 $ Just a
  {-# INLINE gConvertSetter #-}

instance GConvertSetter (K1 i a) (K1 i' (Update a)) where
  gConvertSetter (K1 a) = K1 $ Set a
  {-# INLINE gConvertSetter #-}

instance GConvertSetter (K1 i (Maybe a)) (K1 i' (Update a)) where
  gConvertSetter (K1 (Just a)) = K1 $ Set a
  gConvertSetter (K1 Nothing)  = K1 Unset
  {-# INLINE gConvertSetter #-}

instance GConvertSetter f f' => GConvertSetter (M1 i c f) (M1 i' c' f') where
  gConvertSetter (M1 a) = M1 $ gConvertSetter a
  {-# INLINE gConvertSetter #-}

instance (GConvertSetter a c , GConvertSetter b d) => GConvertSetter (a :*: b) (c :*: d) where
  gConvertSetter (a :*: b) = gConvertSetter a :*: gConvertSetter b
  {-# INLINE gConvertSetter #-}


class GGetEntity a b where
  gGetEntity :: a x -> Int -> b x

instance GGetEntity (K1 i (IntMap a)) (K1 i' (Maybe a)) where
  gGetEntity (K1 a) e = K1 $ I.lookup e $ a
  {-# INLINE gGetEntity #-}

instance GGetEntity (K1 i (Maybe (Int, a))) (K1 i' (Maybe a)) where
  gGetEntity (K1 (Just (e', a))) e | e == e' = K1 $ Just a
  gGetEntity _ _ = K1 Nothing
  {-# INLINE gGetEntity #-}

instance GGetEntity f f' => GGetEntity (M1 i c f) (M1 i' c' f') where
  gGetEntity (M1 a) e = M1 $ gGetEntity a e
  {-# INLINE gGetEntity #-}

instance (GGetEntity a c , GGetEntity b d) => GGetEntity (a :*: b) (c :*: d) where
  gGetEntity (a :*: b) e = gGetEntity a e :*: gGetEntity b e
  {-# INLINE gGetEntity #-}


class GSetEntity a b where
  gSetEntity :: a x -> Int -> b x -> b x

instance GSetEntity (K1 i (Update a)) (K1 i' (Maybe (Int, a))) where
  gSetEntity (K1 (Set a)) e _ = K1 $ Just (e, a)
  gSetEntity (K1 Unset) e (K1 (Just (e', b))) =
    if e == e'
       then K1 Nothing
       else K1 $ Just (e', b)
  gSetEntity _  _ (K1 b) = K1 b
  {-# INLINE gSetEntity #-}

instance GSetEntity (K1 i (Update a)) (K1 i' (IntMap a)) where
  gSetEntity (K1 Keep) _ (K1 b) = K1 b
  gSetEntity (K1 (Set a)) e (K1 b) = K1 $ I.alter (const $ Just a) e b
  gSetEntity (K1 Unset) e (K1 b) = K1 $ I.alter (const Nothing) e b
  {-# INLINE gSetEntity #-}

instance GSetEntity f f' => GSetEntity (M1 i c f) (M1 i' c' f') where
  gSetEntity (M1 a) e (M1 b) = M1 $ gSetEntity a e b
  {-# INLINE gSetEntity #-}

instance (GSetEntity a c , GSetEntity b d) => GSetEntity (a :*: b) (c :*: d) where
  gSetEntity (a :*: b) e (c :*: d) = gSetEntity a e c :*: gSetEntity b e d
  {-# INLINE gSetEntity #-}


def :: forall keep a. (Generic a, GDefault keep (Rep a)) => a
def = to $ gdef @keep
{-# INLINE def #-}


class GDefault (keep :: Bool) f where
  gdef :: f a

instance GDefault keep U1 where
  gdef = U1
  {-# INLINE gdef #-}

instance GDefault keep (K1 i (Maybe c)) where
  gdef = K1 Nothing
  {-# INLINE gdef #-}

instance GDefault 'False (K1 i (Update c)) where
  gdef = K1 Unset
  {-# INLINE gdef #-}

instance GDefault 'True (K1 i (Update c)) where
  gdef = K1 Keep
  {-# INLINE gdef #-}

instance GDefault keep (K1 i (IntMap c)) where
  gdef = K1 I.empty
  {-# INLINE gdef #-}

instance GDefault keep f => GDefault keep (M1 i c f) where
  gdef = M1 $ gdef @keep
  {-# INLINE gdef #-}

instance (GDefault keep a, GDefault keep b) => GDefault keep (a :*: b) where
  gdef = gdef @keep :*: gdef @keep
  {-# INLINE gdef #-}

