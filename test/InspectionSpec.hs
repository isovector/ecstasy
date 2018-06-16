{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -ddump-simpl #-}


module InspectionSpec where

import Control.Monad.Codensity
import Language.Haskell.TH
import GHC.Generics
import Data.Ecstasy
import Test.Inspection
import Test.Hspec

spec :: Spec
spec = pure ()


data World s = World
  { field   :: Component s 'Field   Int
  , unique  :: Component s 'Unique  Bool
  , virtual :: Component s 'Virtual String
  }
  deriving (Generic)

getField :: Monad m => QueryT World m Int
getField = query field

getUnique :: Monad m => QueryT World m Bool
getUnique = query unique

getVirtual :: Monad m => QueryT World m String
getVirtual = query virtual

setter :: World 'SetterOf
setter = unchanged

world :: World ('WorldOf IO)
world = defStorage


inspect $ hasNoGenerics 'getField
inspect $ hasNoType 'getField ''Codensity

inspect $ hasNoGenerics 'getUnique
inspect $ hasNoType 'getUnique ''Codensity

inspect $ hasNoGenerics 'getVirtual
inspect $ hasNoType 'getVirtual ''Codensity

inspect $ hasNoGenerics 'setter
inspect $ hasNoType 'setter ''Codensity

inspect $ hasNoGenerics 'world
inspect $ hasNoType 'world ''Codensity

