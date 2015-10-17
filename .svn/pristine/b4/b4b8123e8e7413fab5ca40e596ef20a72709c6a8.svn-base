module Main where
  --import BreadthFSearch
  --import StateData
  import Data.List    -- \\ の解決のため
  import Util
  import DataType
  import StateData

  -------------------------------------------------------------------
  -- 幅優先探索
  bfsearch :: [State] -> Frontier -> Frontier -> Maybe [Move]
  bfsearch qs [][] = Nothing
  bfsearch qs rs [] = bfsearch qs [ ] rs
  bfsearch qs rs (p@(ms,q) : ps)
    | solved q = Just ms
    | q `elem` qs = bfsearch qs rs ps
    | otherwise = bfsearch (q : qs) (succs p ++rs) ps
  
  bfsolve :: State -> Maybe [Move]
  bfsolve g = bfsearch [] [] [([], g)]
  --------------------------------------------------------------------
  -- 深さ優先探索
  dfsearch :: [State] -> Frontier -> Maybe [Move]
  dfsearch qs [] = Nothing
  dfsearch qs (p@(ms,q) : ps)
    | solved q = Just ms
    | q `elem` qs = dfsearch qs ps
    | otherwise = dfsearch (q : qs) (succs p ++ ps) -- stack
  
  dfsolve :: State -> Maybe [Move]
  dfsolve g = dfsearch [] [([], g)]
  --------------------------------------------------------------------
  -- 計画的探索
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
  -------------------------------------------------------------------------
  
  -- 将来探索予定のパスを作成
  succs :: Path -> [Path]
  succs (ms, q) = [(ms ++ [m], move q m) | m <- moves q]
  
  -- グリッドが占拠しているセルのリスト
  occupied :: Grid -> [Cell]
  occupied = foldr (merge . fillcells) []
  fillcells (r, f) = if r > f-7 then [r .. f] else [r, r+7 .. f]
  
  -- 空いているセル
  freecells :: Grid -> [Cell]
  freecells g = allcells \\ occupied g
  
  allcells = [c | c <- [1 .. 41], c `mod` 7 /= 0]
  
  -- 今の状態で可能な行動のリスト
  moves :: Grid -> [Move]
  moves g = [(v, c) | (v, i) <- zip[0..] g, c <- adjs i, c `elem` fs]
    where fs = freecells g
  
  {--
   [forward Move, back Move]
  --}
  adjs (r, f) = if r > f-7 then [f+1, r-1] else [f+7, r-7]
  
  -- move :: State -> Move -> State
  move :: Grid -> Move -> Grid
  move g (v, c) = g1 ++ adjust i c : g2
    where (g1, i:g2) = splitAt v g
    {-- i が動かす車 (v 番) の (Cell , Cell )
        splitAt でタプルの snd の先頭に目標の車を出すことでアクセスできるようにしている--}
  
  {--
   c が先頭より前なら前に動く
   動いた後の車の位置を出力
  --}
  adjust (r, f) c
    | r > f-7 = if c > f then (r+1, c) else (c, f-1)  -- horizontal
    | otherwise = if c < r then (c, f-7) else (r+7, c)  -- vertical
  
  {--
  脱出させる車は　g の先頭にある
  先頭の座標は Cell の 2 番目に入っている
  先頭の位置が 20 (出口の座標) ならば解けている
  教科書 = 20 -> == 20
  --}
  solved :: Grid -> Bool
  solved g = snd (head g) == 20
