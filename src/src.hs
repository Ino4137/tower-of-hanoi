{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.List (foldl')
import Data.Tuple (swap)
import qualified Data.IntMap as IM

type Tower = ([Int], [Int], [Int])
type Move = (Int, Int)
data MoveP = MP {_re :: [Move], _nt :: [Move], _ars :: (Move, Move)} deriving Show

makeLenses ''MoveP

mirror :: Move -> Move
mirror (x, y)
  -- (1,3) <-> (3,1)
  | abs (x - y) == 2 = (y, x)  
  -- (3,2) <-> (1,2)
  | y == 2           = (mir x, y)
  -- (2,3) <-> (2,1)
  | otherwise        = (x, mir y)
  where
    mir a = (a + 2) `rem` 4

mkTow :: Int -> Tower
mkTow n = ([1..n],[],[])

move :: Tower -> Move -> Tower
move tower (x,y) = 
  let ((this:_),newTow) = repr x <<%~ tail $ tower 
  in repr y %~ (this:) $ newTow
  where
    repr 1 = _1
    repr 2 = _2
    repr 3 = _3

execMoves :: Tower -> [Move] -> Tower
execMoves = foldl' move

toMoves :: MoveP -> [Move]
toMoves (MP [] n _) = n
toMoves (MP r n a) = r ++ [fst a] ++ n ++ [snd a] ++ r 
  ++ [(arrowBT . fst $ a)] ++ n ++ [(arrowBT . snd $ a)] ++ r
    where
      arrowBT = swap 

-- it's not as magical as it looks.
buildMoves :: Int -> [Move] 
buildMoves 1 = [(1,3)]
buildMoves 2 = [(1,2),(1,3),(2,3)]
buildMoves 3 = [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
buildMoves n = toMoves $ fst (go n ima) --snd (go n ima) to see the resulting map
  where
    arrws = ((1,2),(3,2))
    ima = IM.fromList [
        (1, MP [] [(1,3)] ((1,2),(3,2))), 
        (2, MP [] [(1,2),(1,3),(2,3)] ((1,2),(3,2))),
        (3, MP [(1,3)] [(1,3)] ((1,2),(3,2)))
      ]
    go i ima =
      case ima IM.!? i of
        Just x -> (x, ima)
        Nothing ->
          if i /= 4 then
            let (n,ima') = (go (i-1) ima) in
            if i /= 5 then -- N
              let (r,ima'') = (go (i-2) ima')
                  this = MP (toMoves r) (fmap mirror $ _re n) arrws
                  mp = IM.insert i this ima''
              in (this, mp)
            else -- 5
              let r = [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
                  this = MP r (fmap mirror $ _re n) arrws
                  mp =  IM.insert i this ima'
              in (this, mp)   
          else -- 4
            let n = [(3,1)]
                (r,ima') = (go (i-2) ima)
                this = MP (toMoves r) n arrws
                mp =  IM.insert i this ima'
            in (this, mp)

-- primarily to test if the moves are correct
solveTower :: Int -> Tower
solveTower x = execMoves (mkTow x) (buildMoves x) 

main :: IO ()
main = writeFile "res.txt" . show . buildMoves . read =<< getLine