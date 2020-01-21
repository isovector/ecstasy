-- | Ecstasy is a library architected around the
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data/ HKD pattern>, the
-- gist of which is to define a "template" type that can be reused for several
-- purposes. Users of ecstasy should define a record type of 'Component's
-- parameterized over a variable of kind 'StorageType':
--
-- @
--   data World s = Entity
--     { position :: 'Component' s ''Field' (V2 Double)
--     , graphics :: 'Component' s ''Field' Graphics
--     , isPlayer :: 'Component' s ''Unique' ()
--     }
--     deriving ('Generic')
-- @
--
-- Ensure that this type have an instance of 'Generic'.
--
-- For usability, it might be desirable to also define the following type
-- synonym:
--
-- @
--   type Entity = World ''FieldOf'
-- @
--
-- which is the only form of the @World@ that most users of ecstasy will
-- need to interact with.
--
-- Throughout this document there are references to the @HasWorld@ and
-- @HasWorld'@ classes, which are implementation details and provided
-- automatically by the library.
module Data.Ecstasy
  (
  -- * Defining components
  -- $components
    ComponentType (..)
  , Component

  -- * Storage
  -- $world
  , defStorage
  , StorageType (..)

  -- * The SystemT monad
  -- $systemt
  , SystemT ()
  , runSystemT
  , yieldSystemT
  , SystemState

  -- * Working with SystemT
  , createEntity
  , newEntity
  , getEntity
  , setEntity
  , deleteEntity

  -- * SystemT traversals
  -- $traversals
  , emap
  , efor
  , eover
  , unchanged
  , delEntity

  -- * Entity targets
  , EntTarget
  , allEnts
  , uniqueEnt
  , someEnts
  , anEnt
  , entsWith

  -- * The QueryT monad
  -- $queryt
  , QueryT ()
  , runQueryT

  -- * Queries
  -- $querying
  , query
  , subquery
  , with
  , without
  , queryEnt
  , queryUnique
  , querySelf
  , queryMaybe
  , queryFlag
  , queryDef
  , queryTarget

  -- * Updates
  , Update (..)
  , maybeToUpdate

  -- * Introducing effects
  , surgery

  -- * Miscellany
  , Ent ()
  , VTable (..)

  -- * Re-exports
  , Generic ()
  ) where

import Data.Ecstasy.Internal hiding (HasWorld, HasWorld')
import Data.Ecstasy.Types
import GHC.Generics



-- $components
-- Components are pieces of data that may or may not exist on a particular
-- entity. In fact, an 'Ent' is nothing more than an identifier, against which
-- components are linked.
--
-- Components classified by their 'ComponentType', which describes the
-- semantics behind a component.
--
-- [@Field@]  A 'Field' is a "normal" component and corresponds exactly to
-- a 'Maybe' value.
--
-- [@Unique@] A 'Unique' component may only exist on a single entity at a given
-- time. They are often used to annotate "notable" entites, such as whom the
-- camera should be following.
--
-- [@Virtual@] A 'Virtual' component is defined in terms of monadic 'vget' and
-- 'vset' actions, rather than having dedicated storage in the ECS. Virtual
-- components are often used to connect to external systems, eg. to a 3rd party
-- physics engine which wants to own its own data. For more information on
-- using virtual components, see <#world defStorage>.
--
--



-- $world
-- #world# 'defStorage' provides a suitable container for storing entity data, to
-- be used with 'runSystemT' and friends. If you are not using any 'Virtual'
-- components, it can be used directly.
--
-- However, when using 'Virtual' components, the 'VTable' for each must be set
-- on 'defStorage' before being given as a parameter to 'runSystemT'. For
-- example, we can write a virtual 'String' component that writes its updates
-- to stdout:
--
-- @
--   data World s = Entity
--     { stdout :: 'Component' s ''Virtual' String
--     }
--     deriving ('Generic')
--
--  main :: IO ()
--  main = do
--    let storage = 'defStorage'
--          { stdout = 'VTable'
--              { 'vget' = \\_   -> pure Nothing
--              , 'vset' = \\_ m -> for_ m putStrLn
--              }
--          }
--    'runSystemT' storage $ do
--      void $ 'createEntity' 'newEntity'
--        { stdout = Just "hello world"
--        }
-- @
--
-- In this example, if you were to use 'defStorage' rather than @storage@ as the
-- argument to 'runSystemT', you would receive the following error:
--
-- @unset VTable for Virtual component \'stdout\'@



-- $systemt
-- The 'SystemT' transformer provides capabilities for creating, modifying,
-- reading and deleting entities, as well as performing <#traversals query
-- traversals> over them. It is the main monad of ecstasy.



-- $queryt
-- The 'QueryT' transformer provides an environment for <#traversals querying>
-- components of an entity. Due to its 'Control.Monad.MonadPlus' instance,
-- failing queries will prevent further computations in the monad from running.



-- $traversals
-- #traversals# 'SystemT' provides functionality for traversing over entities
-- that match a 'EntTarget' and a <#querying query>. The functions 'emap' and
-- 'eover' return a @world 'SetterOf@, corresponding to partial update of the
-- targeted entity.
--
-- A @world 'SetterOf@ is the world record where all of its selectors have the
-- type @'Update' a@. For example, given a world:
--
-- @
--   data World s = Entity
--     { position :: 'Component' s ''Field' (V2 Double)
--     , graphics :: 'Component' s ''Field' Graphics
--     , isPlayer :: 'Component' s ''Unique' ()
--     }
-- @
--
-- then @World 'SetterOf@ is equivalent to the following definition:
--
-- @
--   data World 'SetterOf = Entity
--     { position :: 'Update' (V2 Double)
--     , graphics :: 'Update' Graphics
--     , isPlayer :: 'Update' ()
--     }
-- @
--
-- 'unchanged' provides a @world 'SetterOf@ which will update no components,
-- and can have partial modifications added to it.
--
-- 'delEntity' provides a @world 'SetterOf@ which will delete all components
-- associated with the targeted entity.



-- $querying
-- #querying# The 'QueryT' monad provides functionality for performing
-- computations over an 'Ent''s components. The basic primitive is 'query',
-- which will pull the value of a component, and fail the query if it isn't
-- set.
--
-- For example, given the following world:
--
-- @
--   data World s = Entity
--     { position :: 'Component' s ''Field' (V2 Double)
--     , velocity :: 'Component' s ''Field' (V2 Double)
--     }
--     deriving ('Generic')
-- @
--
-- we could model a discrete time simulation via:
--
-- @
--   stepTime :: 'System' World ()
--   stepTime = do
--     'emap' 'allEnts' $ do
--       pos <- 'query' position
--       vel <- 'query' velocity
--       pure $ 'unchanged'
--         { position = 'Set' $ pos + vel
--         }
-- @
--
-- which will add an entity's velocity to its position, so long as it has both
-- components to begin with.


