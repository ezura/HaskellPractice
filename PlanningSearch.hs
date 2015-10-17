module PlannigSearch where
  import DataType
  import Main
  import StateData
  import Util
{--
  newplans :: State -> Plan -> [Plan]
  nweplans q ms = mkplans ms
  where
    mkplans ms  | null ms       = []
                | m `elem` qms  = [ms]
                | otherwise     = concat [mkplans (pms ++ ms) |
                                          qms <- premoves q m,
                                          ]
                                          --}
  psearch:: [State] -> AFrontier -> AFrontier -> Maybe [Move]
  psearch qs [][] = Nothing
  psearch qs rs [] = psearch qs [ ] rs
  psearch qs rs (p@(ms,q,plan) : ps)
    | solved q = Just (reverse ms)
    | q `elem` qs = psearch qs rs ps
    | otherwise = psearch (q : qs) (bsuccs p ++ rs) (asuccs p ++ ps)
  
  asuccs, bsuccs :: APath -> [APath]
  asuccs (ms, q, plan)
   = [(ms++[m], move q m, plan_) | m : plan_ <-newplans q plan]
  bsuccs (ms, q, _)
   = [(ms++[m], q_, goalmoves q_) | m <-moves q, let q_ = move q m]

  psolve :: State -> Maybe [Move]
  psolve q = psearch [ ] [ ] [([ ],q,goalmoves q)]

  -- Goal に進むために必要な行動
  goalmoves :: Grid -> Plan
  goalmoves g = [(0, c) | c <- [snd (head g) + 1 .. 20]]
  
  -- 進みたい場所を占拠している車を探す
  blocker :: Grid -> Cell -> (Vehicle, (Cell, Cell))
  blocker g c = search (zip [0..] g) c
  -- 見つかったら車の情報を出力、見つからなかったら 次の車で探す
  --- (車の番号, 車の位置情報 (後ろ, 前))
  search ((v, i) : vis) c = if covers c i then (v, i ) else search vis c
  -- r 番の車が c を占拠しているか
  covers c (r , f) = r <= c && c <= f && (r > f-7 || (c-r) `mod` 7 == 0)
  
  freeingmoves :: Cell -> (Vehicle, (Cell , Cell )) -> [[Move]]
  freeingmoves c (v, (r , f ))
    | r > f-7 = [[(v, j ) | j <-[f +1 .. c+n]] | c+n < k+7] ++
                  [[(v, j ) | j <-[r-1, r-2 .. c-n]] | c-n > k]
    | otherwise = [[(v, j ) | j <-[r-7, r-14 .. c-m]] | c-m > 0] ++
                  [[(v, j ) | j <-[f+7, f+14 .. c+m]] | c+m < 42]
    where (k,m, n) = (f- f `mod` 7, f-r+7, f-r+1)
    
  premoves :: Grid -> Move -> [[Move]]
  premoves g (v, c) = freeingmoves c (blocker g c)
  
  newplans :: Grid -> Plan -> [Plan]
  newplans g [] = []
  newplans g (m : ms) = mkplans (expand g m ++ ms)
    where mkplans ms = if m `elem` gms then [ms] else
                        concat [mkplans (pms ++ ms) |
                        pms <-premoves g m,
                        all (\x -> not $ elem x ms) pms]
                        where m = head ms; gms = moves g
  
  expand :: Grid -> Move -> [Move]
  expand g (v,c)
    | r > f-7 = if c > f then [(v,p) | p <-[f+1 .. c]]
                else [(v,p) | p <-[r-1, r-2 .. c]]
    | otherwise = if c > f then [(v,p) | p <-[f+7,f+14 .. c]]
                  else [(v,p) | p <-[r-7, r-14 .. c]]
      where (r,f) = g !! v
