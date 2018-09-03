{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Ecstasy.Internal.Deriving where

import           Control.Monad.Codensity
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Ecstasy.Types (Update (..), VTable (..), Ent (..), Hooks, defHooks, Component, ComponentType(..), StorageType(..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           Data.Proxy (Proxy (..))
import           GHC.Generics
import           GHC.TypeLits


------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.hoistStorage'.
class GHoistWorld (t :: (* -> *) -> * -> *) (m :: * -> *) a b where
  gHoistWorld :: a x -> b x

instance {-# OVERLAPPING #-} (MonadTrans t, Functor (t m), Monad m)
    => GHoistWorld t m (K1 i (VTable m a)) (K1 i' (VTable (t m) a)) where
  gHoistWorld (K1 (VTable g s)) = K1 $ VTable (fmap lift g) (fmap (fmap lift) s)
  {-# INLINE gHoistWorld #-}

instance {-# OVERLAPPABLE #-} GHoistWorld t m (K1 i a) (K1 i' a) where
  gHoistWorld (K1 a) = K1 a
  {-# INLINE gHoistWorld #-}

instance (Functor (t m), GHoistWorld t m f f')
    => GHoistWorld t m (M1 i c f) (M1 i' c' f') where
  gHoistWorld (M1 a) = M1 $ gHoistWorld @t @m a
  {-# INLINE gHoistWorld #-}

instance (Applicative (t m), GHoistWorld t m a c, GHoistWorld t m b d)
    => GHoistWorld t m (a :*: b) (c :*: d) where
  gHoistWorld (a :*: b) = gHoistWorld @t @m a :*: gHoistWorld @t @m b
  {-# INLINE gHoistWorld #-}


------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.graftStorage'.
class GGraft a b where
  gGraft :: a x -> b x -> a x

instance {-# OVERLAPPING #-} GGraft (K1 i (VTable m a))
                                    (K1 i' (VTable (t m) a)) where
  gGraft a _ = a
  {-# INLINE gGraft #-}

instance GGraft (K1 i a) (K1 i' a) where
  gGraft _ (K1 a) = K1 a
  {-# INLINE gGraft #-}

instance (GGraft f f') => GGraft (M1 i c f) (M1 i' c' f') where
  gGraft (M1 a) (M1 e) = M1 $ gGraft a e
  {-# INLINE gGraft #-}

instance (GGraft a c, GGraft b d) => GGraft (a :*: b) (c :*: d) where
  gGraft (a :*: b) (c :*: d) = gGraft a c :*: gGraft b d
  {-# INLINE gGraft #-}



------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.convertSetter'.
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

instance GConvertSetter f f'
    => GConvertSetter (M1 i c f) (M1 i' c' f') where
  gConvertSetter (M1 a) = M1 $ gConvertSetter a
  {-# INLINE gConvertSetter #-}

instance (GConvertSetter a c, GConvertSetter b d)
    => GConvertSetter (a :*: b) (c :*: d) where
  gConvertSetter (a :*: b) = gConvertSetter a :*: gConvertSetter b
  {-# INLINE gConvertSetter #-}


------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.getEntity'.
class GGetEntity m a b where
  gGetEntity :: a x -> Int -> Codensity m (b x)

instance (Monad m)
    => GGetEntity m (K1 i (VTable m a)) (K1 i' (Maybe a)) where
  gGetEntity (K1 (VTable vget _)) e = lift $ fmap K1 $ vget $ Ent e
  {-# INLINE gGetEntity #-}

instance Applicative m
    => GGetEntity m (K1 i (IntMap a)) (K1 i' (Maybe a)) where
  gGetEntity (K1 a) e = pure . K1 $ I.lookup e $ a
  {-# INLINE gGetEntity #-}

instance Applicative m
    => GGetEntity m (K1 i (Maybe (Int, a))) (K1 i' (Maybe a)) where
  gGetEntity (K1 (Just (e', a))) e | e == e' = pure . K1 $ Just a
  gGetEntity _ _ = pure $ K1 Nothing
  {-# INLINE gGetEntity #-}

instance (Functor m, GGetEntity m f f')
    => GGetEntity m (M1 i c f) (M1 i' c' f') where
  gGetEntity (M1 a) e = fmap M1 $ gGetEntity a e
  {-# INLINE gGetEntity #-}

instance (Applicative m, GGetEntity m a c, GGetEntity m b d)
    => GGetEntity m (a :*: b) (c :*: d) where
  gGetEntity (a :*: b) e = (:*:) <$> gGetEntity a e <*> gGetEntity b e
  {-# INLINE gGetEntity #-}


------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.query'
class GetField (c :: ComponentType) where
  getField
      :: Monad m
      => Component ('WorldOf m) c a
      -> Int
      -> m (Maybe a)

instance GetField 'Field where
  getField c i = pure $ I.lookup i c

instance GetField 'Unique where
  getField c i = pure $ c >>= \(i', a) ->
    case i == i' of
      True  -> Just a
      False -> Nothing

instance GetField 'Virtual where
  getField VTable{vget} i = vget $ Ent i


------------------------------------------------------------------------------
-- | Utility class for implementing 'Data.Ecstasy.Internal.setEntity'.
class GSetEntity m a b where
  gSetEntity :: a x -> Int -> b x -> Codensity m (b x)

instance Applicative m
    => GSetEntity m (K1 i (Update a)) (K1 i' (Maybe (Int, a))) where
  gSetEntity (K1 (Set a)) e _ = pure . K1 $ Just (e, a)
  gSetEntity (K1 (Modify _)) _ _ = error "shit what happens here"
  gSetEntity (K1 Unset) e (K1 (Just (e', b))) =
    pure $ if e == e'
       then K1 Nothing
       else K1 $ Just (e', b)
  gSetEntity _  _ (K1 b) = pure $ K1 b
  {-# INLINE gSetEntity #-}

instance (Monad m)
    => GSetEntity m (K1 i (Update a)) (K1 i' (VTable m a)) where
  gSetEntity (K1 a) e (K1 z@(VTable _ vset)) =
    lift (vset (Ent e) a) *> pure (K1 z)
  {-# INLINE gSetEntity #-}

instance Applicative m
    => GSetEntity m (K1 i (Update a)) (K1 i' (IntMap a)) where
  gSetEntity (K1 Keep) _ (K1 b) = pure $ K1 b
  gSetEntity (K1 (Set a)) e (K1 b) = pure . K1 $ I.alter (const $ Just a) e b
  gSetEntity (K1 Unset) e (K1 b) = pure . K1 $ I.alter (const Nothing) e b
  gSetEntity (K1 (Modify f)) e (K1 b) = pure . K1 $ I.alter (fmap f) e b
  {-# INLINE gSetEntity #-}

instance (Functor m, GSetEntity m f f')
    => GSetEntity m (M1 i c f) (M1 i' c' f') where
  gSetEntity (M1 a) e (M1 b) = fmap M1 $ gSetEntity a e b
  {-# INLINE gSetEntity #-}

instance (Applicative m, GSetEntity m a c, GSetEntity m b d)
    => GSetEntity m (a :*: b) (c :*: d) where
  gSetEntity (a :*: b) e (c :*: d) = (:*:) <$> gSetEntity a e c
                                           <*> gSetEntity b e d
  {-# INLINE gSetEntity #-}


def :: forall keep a. (Generic a, GDefault keep (Rep a)) => a
def = to $ gdef @keep
{-# INLINE def #-}


------------------------------------------------------------------------------
-- | Utility class for implementing various defaults. The 'keep' parameter is
-- used to statically describe whether or not to keep the previous value when
-- dealing with 'Update' fields.
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

instance Monad m => GDefault keep (K1 i (Hooks w m)) where
  gdef = K1 defHooks
  {-# INLINE gdef #-}

instance {-# OVERLAPPING #-} (Applicative m, KnownSymbol sym)
    => GDefault keep (M1 S ('MetaSel ('Just sym) x y z)
                     (K1 i (VTable m a))) where
  gdef = M1 $ K1 $ VTable (const err) (const $ const err)
    where
      err :: err
      err = error $ mconcat
            [ "unset VTable for Virtual component '"
            , symbolVal $ Proxy @sym
            , "'"
            ]
  {-# INLINE gdef #-}

instance GDefault keep f => GDefault keep (M1 i c f) where
  gdef = M1 $ gdef @keep
  {-# INLINE gdef #-}

instance (GDefault keep a, GDefault keep b)
    => GDefault keep (a :*: b) where
  gdef = gdef @keep :*: gdef @keep
  {-# INLINE gdef #-}

