{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Applicative
import Control.Monad.Free
import Data.Char (toUpper)
import Data.Ecstasy
import Data.Ecstasy.Internal.Deriving
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  e <- runSystemT (
      defStorage
      { say = VTable vgetSay vsetSay
      , sAY = VTable vgetSAY vsetSAY
      } ) $ do
    void $ createEntity $ newEntity
        { pos = Just 0
        , vel = Just 1
        , ack = Just True
        , say = Just "hello"
        }

    void $ createEntity $ newEntity
      { pos = Just 0
      , ack = Just False
      , sAY = Just "world"
      }

    let
      step = do
        pos' <- query pos
        vel' <- query vel
        pure $ unchanged
          { pos = Set $ pos' + vel'
          }
    emap allEnts step
    emap allEnts step

    efor allEnts $ do
      i <- queryEnt
      with ack
      pure $ show i

  print e
--   print $ pos e
--   print $ vel e
--   print $ ack e


data Entity f = Entity
  { pos :: Component f 'Field  Int
  , vel :: Component f 'Field  Int
  , ack :: Component f 'Unique Bool
  , say :: Component f 'Virtual String
  , sAY :: Component f 'Virtual String
  } deriving (Generic)

vgetSay _ = pure Nothing
vsetSay _ (Set msg) = putStrLn msg
vsetSay _ _ = pure ()

vgetSAY _ = pure Nothing
vsetSAY _ (Set msg) = putStrLn $ fmap toUpper msg
vsetSAY _ _ = pure ()

zoom :: MonadFree (Zoom Entity m) mf => mf Int
zoom = magic pos


loadQuery :: Monad m => Free (Zoom world m) a -> QueryT world m a
loadQuery = iterM (foldSum go . unZoom)
  where
    go Heckin{..} = do
      f <- query heckinSelector
      maybe empty id $ heckinCont f

