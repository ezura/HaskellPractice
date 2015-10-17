module DepthFSearch (dfsolve) where

  dfsearch :: [State] -> Frontier -> Maybe [Move]
  dfsearch qs [] = Nothing
  dfsearch qs (p@(ms,q) : ps)
    | solved q = Just ms
    | q `elem` qs = dfsearch qs ps
    | otherwise = dfsearch (q : qs) (succs p ++ ps) -- stack

  dfsolve :: State -> Maybe [Move]
  dfsolve g = dfsearch [] [([], g)]