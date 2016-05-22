# Haskell Transducers

Transducers for Haskell. See explanation in my blogpost
[Clojure's Transducers in Haskell](http://hypirion.com/musings/haskell-transducers).

Transducers is used for stream processing, where the stream type is unknown à
priori: It can be list mapping, reductions, used for Conduits, or for your own
types or needs. The input and output stream types are decoupled from eachother.

## Quickstart

The transducers library expose three types: `Reduced`, `Reducer` and
`Transducer`.

The `Reducer` type is a generalisation over the function one would usually pass
to `foldl`, and has the following definition:

```haskell
data Reducer s a b = Reducer { initState :: s,
                               complete :: s -> b -> b,
                               step :: s -> b -> a -> (s, Reduced b)
                             }
```

The `s` in a Reducer is its internal state. `initState` specifies the initial
state of the Reducer, when it has not yet been used.

At the end of the "stream", complete is called with the state and the current
accumulated value, and must return a new accumulated value. Usually you would
pass back the accumulated value (`const id`), but complete can be used for
cleanup or final changes to the value.

The `step` function is called for every element of type `a` in the "stream" or
"process" the Reducer is ran over. It returns the new state, and a Reduced
accumulated result.

The Reduced type describes a return value from step, and its definition is as
follows:

```haskell
data Reduced a = Continue a
               | Reduced a
               deriving (Eq, Ord, Show, Read, Functor)
```

A value of Continue signals that we would like to receive more input, if there
are any. Reduced signals that we will not use further input for anything, and
that the function calling can short-circuit if it would like to.

Reduced is also a comonad, in case you would like to perform extract, duplicate
or extend on it.

A transducer is a function from Reducer to Reducer, and does not care about the
reducing value:

```haskell
{-# LANGUAGE RankNTypes #-}

type Transducer s t a b = forall r. Reducer s a r -> Reducer t b r
```

You should not let transducers specify constraints on `s`, a proper transducer
will work for any `s`.

The transducers library provides a lot of predefined transducers, for example
`map`, `filter`, `take`, `drop`, `partitionBy`, `dedupe`.

To run a reducing function, you would like to use `reduce`: It works like
`foldl'` but for `Reducer`s. You can use `stateless` to make a Reducer that
works like functions passed to foldl':

```haskell
import qualified Data.Transducer as T

T.reduce (T.stateless (+)) == foldl' (+)

T.reduce (T.take 10 $ T.stateless (+)) 0 [1..] == foldl' (+) 0 (take 10 [1..])
```

To run a transducer over a list, use `sequence`:

```haskell
import qualified Data.Transducer as T

*> T.sequence (T.map (+10)) [1..10]
[11,12,13,14,15,16,17,18,19,20]
*> T.sequence T.double [1..10]
[1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]
*> T.sequence T.dedupe [1, 1, 2, 3, 3, 4, 3]
[1,2,3,4,3]
```

You can compose transducers via function composition to create a new transducer.
Note that transducer composition is contravariant, so read evaluation order from
left to right, not right to left:

```haskell
import qualified Data.Transducer as T

*> map (* 4) . map (+ 10) $ [10]
[80]
-- map f . map g $ lst == map (f . g) lst

*> T.sequence (T.map (* 4) . T.map (+ 10)) [10]
[50]
-- T.map f . T.map g == T.map (g . f)

*> T.sequence (T.double . T.take 10) [1..]
[1,1,2,2,3,3,4,4,5,5]
```

Transducers can be converted into conduits by using the `toConduit` function
provided in `Data.Transducer.Conduit`:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Transducer as T
import Data.Transducer.Conduit

*> CL.sourceList [1..] $$ toConduit (T.take 2 . T.double) =$ CL.mapM_ print
1
1
2
2
```

They can also in theory be created from a conduit. Help implementing the
`fromConduit` function is appreciated.

You can also create your own transducers and your own functions that manipulate
them. See
[Clojure's Transducers in Haskell](http://hypirion.com/musings/haskell-transducers)
for more information.

## Real World Production Usage

Use [conduits](https://github.com/snoyberg/conduit) instead. This library will
(most likely) not be published nor maintained.

## License

Copyright © 2016 Jean Niklas L'orange

Distributed under the BSD 3-clause license, which is available in the file
LICENSE.
