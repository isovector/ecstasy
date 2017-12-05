{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE TypeFamilies                 #-}

module Main where

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
        }

    void $ newEntity $ defEntity
      { pos = Just 0
      , ack = Just False
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
  } deriving (Generic)

