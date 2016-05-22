{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Data.Transducer.Conduit
  ( toConduit
  ) where

import Data.Transducer
import Data.Conduit

-- Yields the bs it receives
conduitYielder :: Monad m => Reducer () b (Conduit a m b)
conduitYielder = stateless run
  where run m x = m >> yield x

conduitAwaiter :: Monad m => (Reducer s a (Conduit a m b)) -> Conduit a m b
conduitAwaiter (Reducer is c f) = go is
  where go s = do mval <- await
                  case mval of
                    (Just val) -> feed s val
                    Nothing -> feedLast s
        feed s val = case f s (return ()) val of
                       (s', Reduced comp) -> comp >> feedLast s'
                       (s', Continue comp) -> comp >> go s'
        feedLast s = c s (return ())

-- | toConduit takes a Transducer and converts it into a Conduit for any monad m
toConduit :: Monad m => Transducer () s a b -> Conduit b m a
toConduit xform = conduitAwaiter (xform conduitYielder)
