module RushHours where
  import BreadthFSearch
  import StateData
  
  type Path = ([Move],State)
  type Frontier = [Path]
  
  -- 将来探索予定のパスを作成
  succs :: Path -> [Path]
  succs (ms,q) = [(ms ++ [m], move q m) | m <- moves q]
  
  type Cell = Int
  type Grid = [(Cell, Cell )]
  type Vehicle = Int
  type Move = (Vehicle, Cell)
  type State = Grid     -- 車の位置 = Grid = State
  
  -- グリッドが占拠しているセルのリスト
  occupied :: Grid -> [Cell]
  occupied = foldr (merge · fillcells)[]
  fillcells (r,f) = if r > f−7 then [r ..f] else [r, r+7 ..f]
  --                  horizontal            vertical
  
  ----------------------------------------------------------------
  
  freecells :: Grid -> [Cell]
  freecells g = allcells \\ occupied q
  
  allcells = [c | c <- [1 .. 41], c mod 7 /= 0]
  
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
