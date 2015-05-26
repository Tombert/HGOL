module Main where 



--unique :: [Int] -> [Int]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]


getPossibleAdjacent :: (Integral a) => (a,a) -> [(a,a)]
getPossibleAdjacent (x,y) = [(xa, ya) | xa <- [(x-1)..(x+1)], ya <- [(y-1)..(y+1)], not ((xa, ya) == (x,y))]
 
filterAdjacent :: (Integral a) => ((a,a) -> [(a,a)] -> Bool) -> [(a, a)] -> [(a,a)] -> [(a,a)]
filterAdjacent f x y = [ x' | x' <- x, f x' y]

livingFilter = filterAdjacent elem
nonLivingFilter = filterAdjacent notElem

getLivingAdjacents = livingFilter . getPossibleAdjacent

livingAdjacentCount x xs = length $ getLivingAdjacents x xs

getNonlivingAdjacents = nonLivingFilter . getPossibleAdjacent

nonlivingAdjacentCount x xs = length $ getNonlivingAdjacents x xs


newSet x = remainingLiveCells ++ (unique spawnedDeadCells)
      where 
         remainingLiveCells = [x' | x' <- x, (livingAdjacentCount x' x) == 2 || (livingAdjacentCount x' x) == 3]
         spawnedDeadCells = [y' | x' <- x, y' <- (getNonlivingAdjacents x' x), (livingAdjacentCount y' x) == 3]

main :: IO ()
main = putStrLn "hellojkk"
