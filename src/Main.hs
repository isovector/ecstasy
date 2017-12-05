{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DefaultSignatures            #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE ViewPatterns                 #-}

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
      step e = do
        pos' <- pos e
        vel' <- vel e
        pure $ defEntity'
          { pos = Set $ pos' + vel'
          }
    emap step
    emap step

    efor $ \i e -> do
      acked <- ack e
      posd <- pos e
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

