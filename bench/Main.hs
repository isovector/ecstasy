{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (void, replicateM_)
import           Criterion
import qualified Criterion.Main as C
import           Criterion.Types
import           Data.Ecstasy
import           Data.Ecstasy.Internal
import           Data.Ecstasy.Internal.Deriving

data Entity f = Entity
  { pos    :: Component f 'Field  Int
  , vel    :: Component f 'Field  Int
  , player :: Component f 'Unique ()
  } deriving (Generic)

type Bench = SystemT Entity IO

initSize :: Int -> Bench ()
initSize i = do
  replicateM_ i $
    void $ createEntity $ newEntity
        { pos = Just 0
        , vel = Just 1
        }
  replicateM_ (i * 9) $
    void $ createEntity $ newEntity
        { pos = Just 0
        }
  void $ createEntity $ newEntity
      { pos    = Just 0
      , vel    = Just 1
      , player = Just ()
      }


dumbMap :: Bench ()
dumbMap = emap allEnts $ do
  p <- query pos
  v <- query vel
  pure unchanged
    { pos = Set $ p + v
    }

sparseDumbMap :: Bench ()
sparseDumbMap = emap allEnts $ do
  p <- query pos
  v <- query vel
  _ <- query player
  pure unchanged
    { pos = Set $ p + v
    }

smartMap :: Bench ()
smartMap = esmart allEnts $ do
  p <- magic pos
  v <- magic vel
  pure unchanged
    { pos = Set $ p + v
    }

sparseSmartMap :: Bench ()
sparseSmartMap = esmart allEnts $ do
  p <- magic pos
  v <- magic vel
  _ <- magic player
  pure unchanged
    { pos = Set $ p + v
    }


runWorld :: Bench a -> IO ()
runWorld = runSystemT defStorage . void

runSize :: String -> Int -> Benchmark
runSize name size =
  let initialized = initSize size in
  bgroup name
    [ bench "init" $ whnfIO $ runWorld initialized
    , bench "dumb" $ whnfIO $ runWorld $ initialized >> dumbMap
    , bench "sparse_dumb" $ whnfIO $ runWorld $ initialized >> sparseDumbMap
    , bench "smart" $ whnfIO $ runWorld $ initialized >> smartMap
    , bench "sparse_smart" $ whnfIO $ runWorld $ initialized >> sparseSmartMap
    ]


main :: IO ()
main = C.defaultMainWith C.defaultConfig
  [ runSize "tiny"  10
  , runSize "small" 100
  , runSize "big"   1000
  , runSize "huge"  10000
  ]
