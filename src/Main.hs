{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Applicative
import Control.Monad.Free
import Data.Char (toUpper)
import Data.Ecstasy
import Data.Ecstasy.Internal
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
        pos' <- magic pos
        vel' <- magic vel
        pure $ unchanged
          { pos = Set $ pos' + vel'
          }

    esmart allEnts $ step
    esmart allEnts $ step
    esmart allEnts $ step

    efor allEnts $ do
      i <- queryEnt
      p <- query pos
      pure $ show p

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

vgetSay i = putStrLn ("GET " ++ show i) *> pure Nothing
vsetSay _ (Set msg) = putStrLn msg
vsetSay _ _ = pure ()

vgetSAY _ = pure Nothing
vsetSAY _ (Set msg) = putStrLn $ fmap toUpper msg
vsetSAY _ _ = pure ()

zoom :: MonadFree (Zoom Entity m) mf => mf Int
zoom = magic pos

