{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Data.Transducer
  ( Reduced(..),
    Reducer(..),
    Transducer,
    dedupe,
    double,
    drop,
    dropNth,
    filter,
    map,
    partitionBy,
    preservingReduce,
    reduce,
    sequence,
    stateless,
    take,
  ) where

import Prelude hiding (init, sequence, take, drop, map, pred, filter)
import Control.Comonad

data Reduced a = Continue a
               | Reduced a
               deriving (Eq, Ord, Show, Read, Functor)

instance Comonad Reduced where
  extract (Continue x) = x
  extract (Reduced x) = x

  duplicate (Continue x) = Continue $ Continue x
  duplicate (Reduced x) = Reduced $ Reduced x

data Reducer s a b = Reducer { initState :: s,
                               complete :: s -> b -> b,
                               step :: s -> b -> a -> (s, Reduced b)
                             }

stateless :: (b -> a -> b) -> Reducer () a b
stateless f = Reducer { initState = (),
                        complete = const id,
                        step = \_ x y -> ((), Continue $ f x y)
                      }


reduce' :: (s -> b -> a -> (s, Reduced b)) -> s -> b -> [a] -> (s, b)
reduce' _ state acc [] = (state, acc)
reduce' f state acc (x:xs) = case f state acc x of
                               (s, Reduced v) -> (s, v)
                               (s, Continue v) -> reduce' f s v xs

reduce :: Reducer s a b -> b -> [a] -> b
reduce (Reducer is c f) z as
  = let (state, res) = reduce' f is z as
    in c state res

-- TODO: Possible to ensure t is rigid? Or maybe the state type is necessary
-- here.
type Transducer s t a b = forall r. Reducer s a r -> Reducer t b r

-- This is terrible, and is only used as an example. Please use Data.Sequence
-- instead if you need fast right appends
conj :: [a] -> a -> [a]
conj xs x = xs ++ [x]

listReducer :: Reducer () a [a]
listReducer = stateless conj

sequence :: Transducer () t b a -> [a] -> [b]
sequence xform = go is
  where go s [] = c s []
        go s (x:xs) = case f s [] x of
                        (s', Reduced res) -> res ++ go s' []
                        (s', Continue res) -> res ++ go s' xs
        (Reducer is c f) = xform listReducer

-- NB: This is not a transducer!
preserveReduced :: Reducer s a b -> Reducer s a (Reduced b)
preserveReduced (Reducer is c f) = Reducer { initState = is,
                                             complete = fmap . c,
                                             step = preserveStep
                                           }
  where preserveStep state acc x =
          let (state', v) = f state (extract acc) x
          in (state', duplicate v)

-- Like reduce, but preserves Reduced. Useful if you need to know whether you
-- short-circuited or not.
preservingReduce :: Reducer s a b -> b -> [a] -> Reduced b
preservingReduce f x ys = reduce (preserveReduced f) (Continue x) ys

take :: Int -> Transducer s (Int, s) a a
take n (Reducer is c f) = Reducer { initState = (n, is),
                                      complete = c . snd,
                                      step = takeStep
                                    }
  where takeStep (curN, s) res x
          | 0 < curN = let (s', v) = f s res x in
                      ((curN - 1, s'), v)
          | otherwise = ((curN, s), Reduced res)

drop :: Int -> Transducer s (Int, s) a a
drop n (Reducer is c f) = Reducer { initState = (n, is),
                                    complete = c . snd,
                                    step = dropStep
                                  }
  where dropStep (n, s) res x
          | 0 < n = ((n - 1, s), Continue res)
          | otherwise = let (s', v) = f s res x in
                          ((0, s'), v)

map :: (a -> b) -> Transducer s s b a
map f (Reducer is c stepFn) = Reducer { initState = is,
                                        complete = c,
                                        step = mapStep
                                      }
  where mapStep s res x = stepFn s res (f x)

filter :: (a -> Bool) -> Transducer s s a a
filter pred (Reducer is c stepFn) = Reducer { initState = is,
                                              complete = c,
                                              step = filterStep
                                            }
  where filterStep s acc x
          | pred x = stepFn s acc x
          | otherwise = (s, Continue acc)

partitionBy :: Eq x => (a -> x) -> Transducer s (Maybe ([a], x), s) [a] a
partitionBy pfn (Reducer is c stepFn)
  = Reducer { initState = (Nothing, is),
              complete = partitionComplete,
              step = partitionStep
            }
  where partitionComplete (Nothing, s) x = c s x
        partitionComplete (Just (xs, _), s) acc
          = let (state', res) = stepFn s acc xs
            in c state' (extract res)
        partitionStep (Nothing, s) acc x
          = ((Just ([x], pfn x), s), Continue acc)
        partitionStep (Just (as, cmp), s) acc x
          | cmp == pfn x = ((Just (conj as x, cmp), s),
                            Continue acc)
          | otherwise = let (state', acc') = stepFn s acc as
                        in ((Just ([x], pfn x), state'), acc')

dropNth :: Int -> Transducer s (Int, s) a a
dropNth nInit (Reducer is c stepFn) = Reducer { initState = (nInit - 1, is),
                                                    complete = c . snd,
                                                    step = dropNthStep
                                                  }
  where dropNthStep (n, s) acc x
          | 0 < n = let (s', res) = stepFn s acc x
                    in ((n - 1, s'), res)
          | otherwise = ((nInit - 1, s), Continue acc)

double :: Transducer s s a a
double (Reducer is c f) = Reducer { initState = is,
                                    complete = c,
                                    step = doubleStep
                                  }
  where doubleStep s acc x
          = case f s acc x of
              (s', Continue v) -> f s' v x
              reduced -> reduced

dedupe :: Eq a => Transducer s (Maybe a, s) a a
dedupe (Reducer is c f) = Reducer { initState = (Nothing, is),
                                    complete = c . snd,
                                    step = dedupeStep
                                  }
  where dedupeStep (Nothing, s) acc x
          = let (s', acc') = f s acc x in
              ((Just x, s'), acc')
        dedupeStep (Just x, s) acc v
          | x == v = ((Just x, s), Continue acc)
          | otherwise = dedupeStep (Nothing, s) acc v
