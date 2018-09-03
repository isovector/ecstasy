# Revision history for ecstasy

## 0.3.0.0  -- unreleased

* Replace 'State s' with 'ReaderT (IORef s) IO', resulting in significantly less
    allocations.
* Perform static analysis of 'QueryT's to avoid evaluating them on irrelevant
    entities.

## 0.2.1.0  -- 2018-05-15

* Added the 'surgery' function to introduce temporary effects.
* Significant performance improvements due to constructing monadic generic
    functions via 'Codensity'.

## 0.2.0.1  -- 2018-05-10

* Also export 'StorageType'.

## 0.2.0.0  -- 2018-05-10

* Renamed 'get*' to 'query*'.
* Renamed 'newEntity' to 'createEntity'.
* Renamed 'defEntity' to 'newEntity'.
* Renamed 'defEntity'' to 'unchanged'.
* Renamed 'defWorld' to 'defStorage'.
* Significant performance improvements.
* Added a 'Virtual' component type, allowing for easy integration with systems
    that own their own data. Getting and setting on 'Virtual' components
    dispatch as actions in the underlying monad stack.
* Added proper type wrappers around 'SystemT' and 'QueryT' so they don't eat up
    valuable mtl instances.
* Removed the 'Ent' parameter from the 'efor' callback, since this can now be
    gotten in any 'QueryT' context via 'queryEnt'.
* Parameterized 'emap' and 'efor' by an 'EntityTarget', which allows for calling
    these functions over specific groups of entities.
* Added 'eover': a combination of 'emap' and 'efor'.

## 0.1.1.0  -- 2018-02-18

* Added 'deleteEntity' (function) and 'delEntity' (QueryT setter).

## 0.1.0.1  -- 2018-02-14

* Added 'yieldSystemT' for resuming a 'SystemT' computation later.
* Bumped the upper bound on 'base' to 5 (thanks to nek0).

## 0.1.0.0  -- 2017-12-27

* First version. Released on an unsuspecting world.

