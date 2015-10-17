module BreadthFSearch (bfsolve) where

  bfsearch :: [State] -> Frontier -> Frontier -> Maybe [Move]
  bfsearch qs [][] = Nothing
  bfsearch qs rs [] = bfsearch qs [ ] rs
  bfsearch qs rs (p@(ms,q) : ps)
    | solved q = Just ms
    | q `elem` qs = bfsearch qs rs ps
    | otherwise = bfsearch (q : qs) (succs p ++rs) ps
  
  bfsolve :: State -> Maybe [Move]
  bfsolve = bfsearch [] [] [([], g)]