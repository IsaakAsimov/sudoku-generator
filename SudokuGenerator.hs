import Data.List
import System.Random
import System.Environment

{- Example:
name@debian:~path$ ./SudokuGenerator 30
+---+---+---+---+---+---+---+---+---+
|   |   | 9 | 6 | 8 | 4 |   |   | 3 |
+---+---+---+---+---+---+---+---+---+
| 6 | 2 |   |   |   |   |   | 1 |   |
+---+---+---+---+---+---+---+---+---+
|   | 7 |   |   |   |   |   |   | 4 |
+---+---+---+---+---+---+---+---+---+
|   | 9 |   | 8 |   |   | 7 | 3 |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   | 3 |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
| 7 |   | 8 | 2 | 9 |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   | 5 | 1 | 4 |   |   | 3 | 2 |   |
+---+---+---+---+---+---+---+---+---+
|   | 6 | 2 |   |   | 7 |   | 8 | 1 |
+---+---+---+---+---+---+---+---+---+
| 3 |   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
-}

assoc :: [a] -> Int -> a -> [a]
assoc (x:xs) n e | n == 0 = e:xs
                 | otherwise = x:assoc xs (n-1) e

assocIn :: [[a]] -> (Int, Int) -> a -> [[a]]
assocIn xs (y, x) e = assoc xs y (assoc (xs!!y) x e)

notRepited :: [Int] -> Bool
notRepited = all (==1) . map length . group . sort . filter(/=0)

noIlligal :: [[Int]] -> Bool
noIlligal xs = all notRepited (columns ++ xs ++ boxes)
  where
    columns = [[xs!!b!!a | b <- [0..8]] | a <- [0..8]]
    boxesB = [ (0,2,0,2),(0,2,3,5),(0,2,6,8)
             , (3,5,0,2),(3,5,3,5),(3,5,6,8)
             , (6,8,0,2),(6,8,3,5),(6,8,6,8)]
    boxes  = [[xs!!a!!b | a <- [x..y], b <- [u..v]] | (x,y,u,v) <- boxesB]

findEmpty :: [[Int]] -> (Int, Int)
findEmpty xs = head [(x,y) | x <- [0..8], y <- [0..8], xs !! y !! x == 0]

backtrack :: [[Int]] -> Int -> [Int] -> [[Int]]
backtrack xs n l | not (noIlligal xs) = []
                 | n == 0             = xs
                 | otherwise          = options xs (findEmpty xs) l n l

options :: [[Int]] -> (Int,Int) -> [Int] -> Int -> [Int] -> [[Int]]
options xs (x,y) []     n l = []
options xs (x,y) (o:op) n l = if null sdk
                                 then options xs (x,y) op n l
                                 else sdk
  where
    sdk = backtrack (assocIn xs (y,x) o) (n-1) l

remove :: (RandomGen t) => Int -> [[Int]] -> t -> [[Int]]
remove 0 xs g = xs
remove n xs g = remove (n-1) (assocIn xs (op!!i) 0) ng
  where
    op      = [(y,x) | x <- [0..8], y <- [0..8], xs !! y !! x /= 0]
    (i, ng) = randomR (0, length op - 1) g

-- IO Functions ----------------------------------------------------------------
prints :: [[Int]] -> IO ()
prints xs = mapM_ putStrLn $ line : intersperse line strl2 ++ [line]
  where
    strl2 = map (\x -> "| " ++ intercalate " | " x ++ " |") strl1
    strl1 = map (map (\x -> if x == 0 then " " else show x)) xs
    line     = "+---+---+---+---+---+---+---+---+---+"

main :: IO ()
main = do
  n    <- fmap (read . head) getArgs
  rand <- fmap (randomR (0, 362879 :: Int)) newStdGen
  let emptyBoard =  replicate 9 (replicate 9 0)
      list       =  permutations [1..9] !! fst rand
      sdk        =  backtrack emptyBoard 81 list

  prints $ remove (81 - n) sdk (snd rand)
