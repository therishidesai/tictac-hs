{-# LANGUAGE LambdaCase#-}

module Main where

import Debug.Trace

import Data.Vector as V

data Piece = X | O | Empty deriving Show;

data Meta = Meta {
  row :: Vector Int,
  col :: Vector Int,
  diag :: Int,
  adiag :: Int,
  win :: Bool
  } deriving Show

data Board = Board {
  size :: Int,
  vec :: Vector Piece,
  xMeta :: Meta,
  yMeta :: Meta
  } deriving Show

initMeta :: Int -> Meta
initMeta n = Meta {
  row = generate n (const 0),
  col = generate n (const 0),
  diag = 0,
  adiag = 0,
  win = False
  }

accumMeta :: Meta -> Int -> Int -> Int -> Meta
accumMeta (Meta r c d ad _) n x y = let
    nr = accum (+) r [(x,1)]
    nc = accum (+) c [(y,1)]
    nd = if x == y then d + 1 else d
    nad = if x + y == n - 1 then ad + 1 else ad
  in Meta {
    row = nr,
    col = nc,
    diag = nd,
    adiag = nad,
    win = (!) nr x == n || (!) nc y == n || nd == n || nad == n
  }

updateMeta :: Piece -> Board -> Int -> Int -> Board
updateMeta X (Board n v mm my) x y = let
  m = accumMeta mm n x y
 in Board {
  size = n,
  vec = v,
  xMeta = m,
  yMeta = my
 }
updateMeta O (Board n v mx mm) x y = let
  m = accumMeta mm n x y
 in Board {
  size = n,
  vec = v,
  xMeta = mx,
  yMeta = m
 }
updateMeta Empty b _ _ = b

createBoard :: Int -> Board
createBoard n = Board {
  size = n,
  vec = generate (n * n) (const Empty),
  xMeta = initMeta n,
  yMeta = initMeta n}

printBoard :: Board -> IO ()
printBoard (Board n v _ _) = do
  putStrLn "-----------------"
  V.imapM_ (\i x -> do
               case x of
                 Empty -> do
                   putStr (show i)
                 _ -> do
                   putStr (show x)
               putStr " "
               if (i + 1) `mod` n == 0
                 then putStrLn ""
               else putStr ""
         ) v
  putStrLn "----------------"

getSquare :: Board -> Int -> Int -> Maybe Piece
getSquare (Board n v _ _) x y = let i = x * n + y
  in (!?) v i

setSquare :: Piece -> Board -> Int -> Int -> Maybe Board
setSquare p (Board n v xm ym) x y =  let i = x * n + y
  in aux i $ getSquare (Board n v xm ym) x y
  where aux i (Just Empty) = Just (updateMeta p (Board { size = n, vec = v // [(i, p)], xMeta = xm, yMeta = ym}) x y)
        aux _ _ = Nothing

xWon :: Board -> Bool
xWon (Board _ _ (Meta _ _ _ _ w) _ ) = w

oWon :: Board -> Bool
oWon (Board _ _ _ (Meta _ _ _ _ w) ) = w

isDone :: Board -> (Bool, Bool, Bool)
isDone b = let
    Board _ v _ _ = b
    v' = V.filter (\case Empty -> True
                         _ -> False) v
  in
  (xWon b, oWon b, V.null v')

minimax :: Board -> Int -> Bool -> Int
minimax b d mp = let (p, o, t) = isDone b
                     Board nn _ _ _ = b
                     n = ((nn*nn) - 1)
             in
              if p
                 then 10 - d
              else if o
                 then (-10) + d
              else if t
                 then 0
              else
                 if mp
                    then Prelude.foldr (\i prev ->  let (x,y) = getCoord i b
                                     in case setSquare O b x y of
                                       Just b' -> min prev (minimax b' (d + 1) False)
                                       Nothing -> prev) 1000 [0..n]
                 else
                   Prelude.foldr (\i prev ->  let (x,y) = getCoord i b
                                     in case setSquare X b x y of
                                       Just b' -> max prev (minimax b' (d + 1) True)
                                       Nothing -> prev) (-1000) [0..n]

botmove' :: Board -> (Int, Board)
botmove' b = let Board nn _ _ _ = b
                 n' = ((nn*nn) - 1)
                 n = trace ("n " Prelude.++ show n') n'
                 nb = Prelude.foldr (\i (prev, bb) ->  let (x,y) = getCoord i b
                             in case setSquare O b x y of
                                  Just b' -> let mm = minimax b' 1 False
                                             in
                                               if mm < prev
                                               then (mm, b')
                                               else (prev, bb)
                                  Nothing -> (prev, bb) ) (1000, b) [0..n]
  in
    nb

botmove :: Board -> Board
botmove b = let Board nn _ _ _ = b
                n' = ((nn*nn) - 1)
                n = trace ("n " Prelude.++ show n') n'
                (_, nb) = Prelude.foldr (\i (prev, bb) ->  let (x,y) = getCoord i b
                             in case setSquare O b x y of
                                  Just b' -> let mm = minimax b' 1 False
                                             in
                                               if mm < prev
                                               then (mm, b')
                                               else (prev, bb)
                                  Nothing -> (prev, bb) ) (1000, b) [0..n]
  in
    nb
  
getCoord :: Int -> Board -> (Int, Int)
getCoord p (Board n _ _ _) = ( p `div` n, p `mod` n)

playGame :: Board -> IO()
playGame b = let (p,o,t) = isDone b
             in do
  if p
    then putStrLn "X won"
  else if o
    then putStrLn "O won"
  else if t
    then putStrLn "It's a Tie"
  else do
    printBoard b
    putStrLn "Make a move"
    putStrLn "pos = "
    pp <- getLine
    let (x,y) = getCoord (read pp :: Int) b
    print x
    print y
    let b' = setSquare X b x y
    case b' of
        Just bb -> do
                printBoard bb
                let nb = botmove bb
                printBoard nb
                playGame nb
        Nothing -> do
                putStrLn "Illegal Move"
                playGame b

main :: IO ()
main = do
  let b = createBoard 3
  playGame b

