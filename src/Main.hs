{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Data.Char (toUpper)
import Data.Ecstasy
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


