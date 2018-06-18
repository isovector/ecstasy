{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Ecstasy.Internal.Deriving where

import Control.Arrow (first)
import Control.Monad (join)
import Control.Monad.Free
import           Control.Monad.Codensity
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Ecstasy.Types (Update (..), VTable (..), Ent (..), StorageType (..), ComponentType (..), Component)
import           Data.Extensible.Sum
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
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
-- | Utility class for implementing 'Data.Ecstasy.Internal.setEntity'.
class GSetEntity m a b where
  gSetEntity :: a x -> Int -> b x -> Codensity m (b x)

instance Applicative m
    => GSetEntity m (K1 i (Update a)) (K1 i' (Maybe (Int, a))) where
  gSetEntity (K1 (Set a)) e _ = pure . K1 $ Just (e, a)
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


------------------------------------------------------------------------------
-- |

class GCommand w i o where
  type Commands w i o :: [*]
  gCommand :: (w -> i x) -> o x


instance GCommand w (K1 _i (IntMap a))
                    (K1 _i' (w -> (IntMap a, Maybe IntSet))) where
  type Commands w
               (K1 _i (IntMap a))
               (K1 _i' (w -> (IntMap a, Maybe IntSet))) = '[a]
  gCommand f = K1 $ \w ->
    let v = unK1 $ f w
     in (v, Just $ I.keysSet v)
  {-# INLINE gCommand #-}

instance GCommand w (K1 _i (Maybe (Int, a)))
                    (K1 _i' (w -> (Maybe (Int, a), Maybe IntSet))) where
  type Commands w
               (K1 _i (Maybe (Int, a)))
               (K1 _i' (w -> (Maybe (Int, a), Maybe IntSet))) = '[a]
  gCommand f = K1 $ \w ->
    let v = unK1 $ f w
     in (v, Just $ maybe IS.empty (IS.singleton . fst) v)
  {-# INLINE gCommand #-}

instance GCommand w i o => GCommand w (M1 _i _c i) (M1 _i' _c' o) where
  type Commands w (M1 _i _c i) (M1 _i' _c' o) = Commands w i o
  gCommand f = M1 . gCommand $ unM1 . f
  {-# INLINE gCommand #-}

instance (GCommand w i o, GCommand w i' o')
    => GCommand w (i :*: i') (o :*: o') where
  type Commands w (i :*: i') (o :*: o') = Commands w i o :++ Commands w i' o'
  gCommand f = gCommand ((\(a :*: _) -> a) . f)
           :*: gCommand ((\(_ :*: b) -> b) . f)
  {-# INLINE gCommand #-}


command
    :: forall world m worldOf
     . ( GCommand (world ('WorldOf m))
                  (Rep worldOf)
                  (Rep (world ('FreeOf worldOf)))
       , Generic worldOf
       , Generic (world ('FreeOf worldOf))
       , worldOf ~ world ('WorldOf m)
       )
    => world ('FreeOf (world ('WorldOf m)))
command = to $ gCommand @worldOf from
{-# INLINE command #-}


 -- world ('WorldOf m) -> Component ('FreeOf (world ('WorldOf m))) z a

magic
    :: forall world m a
     . ( GCommand (world ('WorldOf m))
                  (Rep (world ('WorldOf m)))
                  (Rep (world ('FreeOf (world ('WorldOf m)))))
       , Generic (world ('WorldOf m))
       , Generic (world ('FreeOf (world ('WorldOf m))))
       )
    => ( world ('FreeOf (world ('WorldOf m)))
      -> Component ('FreeOf (world ('WorldOf m))) 'Field a
       )
    -> Free (Zoom world m) a
magic f =
  let sel = f $ command @world
      in liftF . Zoom . EmbedAt undefined $ Heckin (sel) Just


type family xs :++ ys where
  '[] :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

newtype Zoom w m a = Zoom
  { unZoom
      :: Heckin (w ('WorldOf m)) a
      :| Commands (w ('WorldOf m))
                  (Rep (w ('WorldOf m)))
                  (Rep (w ('FreeOf (w ('WorldOf m)))))
  }

instance Functor (Zoom w f) where
  fmap f (Zoom (EmbedAt m (Heckin r k))) =
    Zoom . EmbedAt m . Heckin r $ fmap f . k
  {-# INLINE fmap #-}

data Heckin w b a = Heckin
  { heckinRelevant :: w -> (Maybe a, Maybe IntSet)
  , heckinCont     :: a -> Maybe b
  }

-- -- zoo2 :: Codensity (Free (Flip (->)

