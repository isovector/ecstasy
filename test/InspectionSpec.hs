{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- {-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}


module InspectionSpec where


import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Extensible.Sum
import Data.Extensible.Internal
import Control.Monad.Free
import Control.Monad.Codensity
-- import Language.Haskell.TH
import GHC.Generics
import Data.Ecstasy
import Data.Ecstasy.Internal.Deriving
-- import Test.Inspection
-- import Test.Hspec

-- spec :: Spec
-- spec = pure ()


data World s = World
  { field   :: Component s 'Field   Int
  , unique  :: Component s 'Unique  Bool
  -- , virtual :: Component s 'Virtual String
  }
  deriving (Generic)

data WorldF m a
  = FieldF   (World ('WorldOf m) -> IntSet) (Int -> a)
  | UniqueF  (World ('WorldOf m) -> IntSet) (Bool -> a)
  -- | VirtualF (World ('WorldOf m) -> IntSet) (String -> a)


getField :: Monad m => QueryT World m Int
getField = query field

getUnique :: Monad m => QueryT World m Bool
getUnique = query unique

-- getVirtual :: Monad m => QueryT World m String
-- getVirtual = query virtual

setter :: World 'SetterOf
setter = unchanged

world :: World ('WorldOf IO)
world = defStorage


-- inspect $ hasNoGenerics 'getField
-- inspect $ hasNoType 'getField ''Codensity

-- inspect $ hasNoGenerics 'getUnique
-- inspect $ hasNoType 'getUnique ''Codensity

-- inspect $ hasNoGenerics 'getVirtual
-- inspect $ hasNoType 'getVirtual ''Codensity

-- inspect $ hasNoGenerics 'setter
-- inspect $ hasNoType 'setter ''Codensity

-- inspect $ hasNoGenerics 'world
-- inspect $ hasNoType 'world ''Codensity

zoo
    :: ( Monad m
       )
    => Free (Zoom World m) Bool
zoo = do
  magic unique

-- magic
--     :: forall a sym
--      . Member (GCommand (Rep (World 'FreeOf)))
--               (Tagged sym a)
--     => (World 'FreeOf -> Tagged sym a)
--     -> Free (Zoom (World 'FreeOf)) a
-- magic f = liftF
--         $ Zoom
--         $ embed
--         $ Flip
--         $ id @a

