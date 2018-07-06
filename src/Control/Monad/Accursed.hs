{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Accursed where

import           Control.Applicative (Alternative (..))
import           Control.Exception (Exception, throw, catch)
import           Control.Monad.Codensity (improve, lowerCodensity)
import qualified Control.Monad.Free as F
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Writer.Strict (runWriter, tell)
import           GHC.Generics
import           GHC.TypeLits
import           System.IO.Unsafe (unsafePerformIO)



------------------------------------------------------------------------------
-- | The 'Accursed' monad in which evaluation of 'curse' will be interpreted as
-- 'empty' at the time it happens. Under very specific circumstances, this
-- allows some degree of static analysis over free monads. The rest of the time
-- it will lead to terror, madness and runtime crashes.
data Accursed f a
  = Pure a
  | Free (f (Accursed f a))
  | Empty
  deriving (Generic, Generic1)

instance Functor f => Functor (Accursed f) where
  fmap f (Pure a)  = unsafePerformIO $
    catch
      (let !_ = a
        in pure $ Pure $ f a)
      (\(_ :: Curse) -> pure $ Empty)
  fmap f (Free fa) = Free (fmap f <$> fa)
  fmap _ Empty     = Empty
  {-# INLINE fmap #-}

instance Functor f => Applicative (Accursed f) where
  pure = Pure
  {-# INLINE pure #-}
  Empty  <*> _ = Empty
  Pure a <*> Pure b = unsafePerformIO $
    catch
      (let !_ = a
           !_ = b
        in pure $ Pure $ a b)
      (\(_ :: Curse) -> pure $ Empty)
  Pure a <*> Free mb = unsafePerformIO $
    catch
      (let !_ = a
           !_ = mb
        in pure $ Free $ fmap a <$> mb)
      (\(_ :: Curse) -> pure $ Empty)
  Free ma <*> b = unsafePerformIO $
    catch
      (let !_ = ma
           !_ = b
        in pure $ Free $ (<*> b) <$> ma)
      (\(_ :: Curse) -> pure $ Empty)
  _ <*> Empty = Empty
  {-# INLINE (<*>) #-}

instance Functor f => Monad (Accursed f) where
  return = pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = unsafePerformIO $
    catch
      (let !_ = m
        in pure $ Free ((>>= f) <$> m))
      (\(_ :: Curse) -> pure $ Empty)
  Empty >>= _ = Empty
  {-# INLINE (>>=) #-}

instance Functor f => Alternative (Accursed f) where
  empty = Empty
  Empty <|> a = a
  a     <|> _ = a


------------------------------------------------------------------------------
-- | Lift a natural transformation from 'f' to 'g' over 'Accursed'.
hoistAccursed
    :: Functor g
    => (forall x. f x -> g x)
    -> Accursed f a
    -> Accursed g a
hoistAccursed _ (Pure a)   = Pure a
hoistAccursed nat (Free f) = Free $ fmap (hoistAccursed nat) $ nat f
hoistAccursed _ Empty      = Empty


------------------------------------------------------------------------------
-- | Perform a best-effort analysis of a free monad.
analyze
    :: Functor f
    => (forall b. f (Accursed f b) -> Accursed f b)
       -- ^ The following function. Consider using 'channel' to get an
       -- automatic implementation for this.
    -> Accursed f a
    -> [f ()]
analyze c = snd . runWriter . runMaybeT . go
  where
    go Empty = empty
    go (Pure a) = unsafePerformIO $
      catch
        (let !_ = a
          in pure $ pure a)
        (\(_ :: Curse) -> pure $ empty)
    go (Free f) = do
      tell . pure $ () <$ f
      unsafePerformIO $
        catch
          ( let !z = c f
             in pure $ go z)
          (\(_ :: Curse) -> pure $ empty)
    {-# INLINE go #-}


------------------------------------------------------------------------------
-- | The underlying machinery of 'curse'.
data Curse = Curse
  deriving Show
instance Exception Curse


------------------------------------------------------------------------------
-- | A 'curse' is unholy tretchery whose evaluation can be caught in the
-- 'Accursed' monad. It can be used to follow continuations in a free monad
-- until it branches.
curse :: Functor f => Accursed f a
curse = pure $ throw Curse


------------------------------------------------------------------------------
-- | Convert a 'F.Free' monad into an 'Accursed' monad.
corrupt
    :: Functor f
    => F.Free f a
    -> Accursed f a
corrupt (F.Pure a) = Pure a
corrupt (F.Free f) = Free $ fmap corrupt f


------------------------------------------------------------------------------
-- | Helper class to derive 'channel' generically.
class GChannel p f where
  gchannel :: f (Accursed p b) -> Accursed p b

instance TypeError
    ( Text "Missing continuation parameter when attempting to derive 'channel'"
 :$$: Text "Expected a type variable, but got "
 :<>: ShowType a)
      => GChannel p (K1 _1 a) where
  gchannel = undefined
  {-# INLINE gchannel #-}

instance {-# OVERLAPPING #-} TypeError
    ( Text "Missing continuation parameter when attempting to derive 'channel'"
 :$$: Text "Expected a type variable, but the constructor '"
 :<>: Text tyConName
 :<>: Text "' has none")
      => GChannel p (C1 ('MetaCons tyConName _b _c) U1) where
  gchannel = undefined
  {-# INLINE gchannel #-}

instance GChannel p V1 where
  gchannel _ = undefined
  {-# INLINE gchannel #-}

instance Functor p => GChannel p (Rec1 ((->) a)) where
  gchannel (Rec1 z) = do
    c <- curse
    z c
  {-# INLINE gchannel #-}

instance GChannel p Par1 where
  gchannel (Par1 z) = z
  {-# INLINE gchannel #-}

instance GChannel p g => GChannel p (f :*: g) where
  gchannel (_ :*: b) = gchannel b
  {-# INLINE gchannel #-}

instance (GChannel p f, GChannel p g) => GChannel p (f :+: g) where
  gchannel (L1 f) = gchannel f
  gchannel (R1 g) = gchannel g
  {-# INLINE gchannel #-}

instance GChannel p f => GChannel p (M1 _1 _2 f) where
  gchannel (M1 f) = gchannel f
  {-# INLINE gchannel #-}


------------------------------------------------------------------------------
-- | Generically derived continuation follower; intended to be used as the
-- first parameter for 'analyze'.
channel
    :: (Generic1 f, GChannel f (Rep1 f))
    => f (Accursed f a)
    -> Accursed f a
channel = gchannel . from1
{-# INLINE channel #-}



-- cont :: F.MonadFree Pattern m => m Bool
-- cont = F.liftF $ Cont id

-- action :: F.MonadFree Pattern m => Int -> m ()
-- action i = F.liftF $ Action i ()


-- spook
--     :: (Functor f, Alternative f)
--     => (forall m. (F.MonadFree f m, Alternative m) => m a)
--     -> Accursed f a
-- spook f = corrupt $ lowerCodensity f


-- zoom :: [Pattern ()]
-- zoom = analyze channel . corrupt $ improve $ do
--   x <- cont
--   action 5
--   y <- cont
--   if x
--      then pure True
--      else cont



-- data Pattern a
--   = Cont (Bool -> a)
--   | Action Int a
--   deriving (Functor, Generic, Generic1)

-- instance Show (Pattern a) where
--   show (Cont _) = "Cont"
--   show (Action _ _ ) = "Action"

