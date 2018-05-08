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
  e <- runSystemT defWorld $ do
    void $ newEntity $ defEntity
        { pos = Just 0
        , vel = Just 1
        , ack = Just True
        , say = Just "hello"
        }

    void $ newEntity $ defEntity
      { pos = Just 0
      , ack = Just False
      , sAY = Just "world"
      }

    let
      step = do
        pos' <- get pos
        vel' <- get vel
        pure $ defEntity'
          { pos = Set $ pos' + vel'
          }
    emap step
    emap step

    efor $ \i -> do
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
  , say :: Component f ('Virtual "lower") String
  , sAY :: Component f ('Virtual "upper") String
  } deriving (Generic)

instance VirtualAccess "lower" IO [Char] where
  vget _ = pure Nothing
  vset _ (Set msg) = putStrLn msg
  vset _ _ = pure ()

instance VirtualAccess "upper" IO [Char] where
  vget _ = pure Nothing
  vset _ (Set msg) = putStrLn $ fmap toUpper msg
  vset _ _ = pure ()


