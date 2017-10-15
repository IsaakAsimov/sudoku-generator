import Data.List
import System.Random
import System.Environment (getArgs)
import Shuffle (shuffle)

type Grid       = [[Int]]
type Coordinate = (Int, Int)

assoc :: [a] -> Int -> a -> [a]
assoc (x:xs) n e | n == 0 = e:xs
                 | otherwise = x:assoc xs (n-1) e

assocIn :: [[a]] -> Coordinate -> a -> [[a]]
assocIn xs (x, y) e = assoc xs x (assoc (xs!!x) y e)

notRepited :: [Int] -> Bool
notRepited = all (==1) . map length . group . sort . filter(/=0)

noIlligal :: Grid -> Coordinate -> Bool
noIlligal grid (x,y) = all notRepited [column, block, grid!!x]
  where
    column  = [grid!!a!!y | a <- [0..8]]
    blocksB = [ [((0,2), (0,2)), ((0,2), (3,5)), ((0,2), (6,8))]
              , [((3,5), (0,2)), ((3,5), (3,5)), ((3,5), (6,8))]
              , [((6,8), (0,2)), ((6,8), (3,5)), ((6,8), (6,8))]]
    block   = [grid!!a!!b | a <- [g1..g2], b <- [h1..h2]]
    ((g1, g2), (h1, h2)) = blocksB !! div x 3 !! div y 3

findEmpty :: Grid -> Coordinate
findEmpty xs = head [(x,y) | x <- [0..8], y <- [0..8], xs !! x !! y == 0]

backtrack :: Grid -> Coordinate -> Int -> [Int] -> StdGen -> Grid
backtrack xs coor n l gen
        | not (noIlligal xs coor) = []
        | n == 0                  = xs
        | otherwise               = options xs nextCoor l n newl newGen
  where
    (newl, newGen) = shuffle l gen
    nextCoor       = findEmpty xs

options :: Grid -> Coordinate -> [Int] -> Int -> [Int] -> StdGen -> Grid
options xs (x,y) []     n l gen = []
options xs (x,y) (o:op) n l gen = if null sdk
                                 then options xs (x,y) op n l gen
                                 else sdk
  where
    sdk = backtrack (assocIn xs (x,y) o) (x,y) (n-1) l gen

remove :: Int -> Grid -> StdGen -> Grid
remove 0 xs g = xs
remove n xs g = remove (n-1) (assocIn xs (op!!i) 0) ng
  where
    op      = [(x,y) | x <- [0..8], y <- [0..8], xs !! x !! y /= 0]
    (i, ng) = randomR (0, length op - 1) g

-- IO Functions ----------------------------------------------------------------
printGrid :: Grid -> IO ()
printGrid xs = mapM_ putStrLn $ line : intersperse line strl2 ++ [line]
  where
    strl2 = map (\x -> "| " ++ intercalate " | " x ++ " |") strl1
    strl1 = map (map (\x -> if x == 0 then " " else show x)) xs
    line     = "+---+---+---+---+---+---+---+---+---+"

main :: IO ()
main = do
  let emptyBoard = replicate 9 (replicate 9 0)
  n           <- fmap (read . head) getArgs
  (list, gen) <- fmap (shuffle [1..9]) newStdGen
  sdk         <- fmap (backtrack emptyBoard (0,0) 81 list) newStdGen

  printGrid $ remove (81 - n) sdk gen
