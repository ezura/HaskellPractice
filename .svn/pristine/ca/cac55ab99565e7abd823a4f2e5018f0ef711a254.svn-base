module Util where
  import Control.Monad
  
  merge :: [a] -> [a] -> [a]
  merge xs     []     = xs
  merge []     ys     = ys
  merge (x:xs) (y:ys) = x : y : merge xs ys
  
  -- モナドの中のリストの長さを調べる
  maybeLength :: Maybe [a] -> Maybe Int
  maybeLength = liftM length
  {--
  *Main> import Control.Monad
  *Main Control.Monad> liftM length $ bfsolve g1
  Just 34
  *Main Control.Monad> liftM length $ dfsolve g1
  Just 1228
  --}
