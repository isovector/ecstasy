{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Accursed where

import           Control.Applicative (Alternative (..))
import           Control.Exception (Exception, throw, catch)
import qualified Control.Monad.Free as F
import Control.Monad.Codensity (improve, lowerCodensity)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Writer.Strict (runWriter, tell, guard)
import           System.IO.Unsafe (unsafePerformIO)



------------------------------------------------------------------------------
-- | The 'Accursed' monad in which evaluation of 'curse' will be interpreted as
-- 'empty' at the time it happens. Under very specific circumstances, this
-- allows some degree of static analysis over free monads. The rest of the time
-- it will lead to terrible, unutterable bugs.
data Accursed f a
  = Pure a
  | Free (f (Accursed f a))
  | Empty


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


analyze
    :: (Functor f)
    => (forall b. f (Accursed f b) -> (forall z. z) -> Accursed f b)
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
          ( let !z = c f curse
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
curse :: a
curse = throw Curse


accurse
    :: Functor f
    => F.Free f a
    -> Accursed f a
accurse (F.Pure a) = Pure a
accurse (F.Free f) = Free $ fmap accurse f

sanctify
    :: (Functor f, Alternative f)
    => Accursed f a
    -> F.Free f a
sanctify (Pure a) = F.Pure a
sanctify (Free f) = F.Free $ fmap sanctify f
sanctify Empty    = empty


-- cont :: Accursed Pattern Bool
-- cont = Free . fmap pure $ Cont id

cont :: F.MonadFree Pattern m => m Bool
cont = F.liftF $ Cont id


spook
    :: (Functor f, Alternative f)
    => (forall m. (F.MonadFree f m, Alternative m) => m a)
    -> Accursed f a
spook f = accurse $ lowerCodensity f


zoom :: [Pattern ()]
zoom = analyze zoo $ accurse $ improve $ do
  x <- cont
  y <- cont
  if x
     then pure True
     else cont
  where
    zoo (Cont !k) z = k z



data Pattern a
  = Cont (Bool -> a)
  deriving (Functor)

instance Show (Pattern a) where
  show (Cont _) = "Cont"

