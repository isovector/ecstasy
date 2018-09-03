# Ecstasy

[![Build Status](https://travis-ci.org/isovector/ecstasy.svg?branch=master)](https://travis-ci.org/isovector/ecstasy) | [Hackage][hackage]

[hackage]: https://hackage.haskell.org/package/ecstasy

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

