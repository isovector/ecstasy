# Ecstasy

[![Build Status](https://travis-ci.org/isovector/ecstasy.svg?branch=master)](https://travis-ci.org/isovector/ecstasy) | [Hackage][hackage]

[hackage]: https://hackage.haskell.org/package/ecstasy-0.1.0.0

## Dedication

> When I see things like this, there’s nothing better than the underlying
> feeling that I’m a part of something good, some incredible positive vibration
> that we’re somehow all spinning around on. There’s nothing better than to
> think about all those people who are worried about me, and to understand
> they’re concerned, fundamentally, out of love. There’s nothing better than to
> know that there’s people who care that much. But, most of all, there’s nothing
> to better than to know that those people are wrong. Am I out of control? Fuck
> no, I’m a goddamn Jedi. That’s as in control as it gets.
>
> Ben Kenobi, Erowid


## Overview

Ecstasy is an *entity-component system* for Haskell. It's inspired by
[apecs][apecs], but makes the design decision to focus on being idiomatic rather
than being fast. Maybe. I haven't actually benchmarked it.

[apecs]: https://github.com/jonascarpay/apecs

We achieve being idiomatic by using GHC.Generics and tricky type families to
derive performant data stores given only a record of the desired components.


## Example

We can create a simple kinematics simulator:

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Ecstasy


data Entity' f = Entity
  { pos    :: Component f 'Field (V2 Float)
  , vel    :: Component f 'Field (V2 Float)
  , acc    :: Component f 'Field (V2 Float)
  , noGrav :: Component f 'Field ()
  } deriving (Generic)


simulate :: Entity' 'WorldOf -> Entity' 'WorldOf
simulate = flip runSystem $ do
  -- add gravity to anything affected by it
  emap $ do
    without noGrav  -- only run this code for entities without 'noGrav' set
    a <- getMaybe (V2 0 0) <$> getMaybe acc
    pure defEntity'
      { acc = Set $ a + V2 0 9.8 }

  -- integrate acceleration into velocity
  emap $ do
    v <- get vel
    a <- get acc
    pure defEntity'
      { vel = Set $ v + a }

  -- integrate velocity into position
  emap $ do
    p <- get pos
    v <- get vel
    pure defEntity'
      { pos = Set $ p + v }

  getWorld
```

