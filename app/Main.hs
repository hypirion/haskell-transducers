module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Transducer as T
import Data.Transducer.Conduit

-- This is the current pain of this version of Haskell transducers: You have to
-- manually write the state types. It's not a problem, because you would always
-- depend on a rigid input state type, but it's hairy when you want to add type
-- signatures.
myXform :: T.Transducer s (Int, (Maybe ([Int], Int), s)) [Int] Int
myXform =  T.take 10 . T.map f . T.partitionBy (`mod` 3)
  where f x = 3 * x^3 - 2 * x^2 + 2 * x

main :: IO ()
main = do
  mapM_ print $ T.sequence myXform [1..]
  CL.sourceList [1..] $$ toConduit myXform =$ CL.mapM_ print
