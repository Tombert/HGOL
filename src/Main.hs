module Main where 

getPossibleAdjacent :: (Integral a) => (a,a) -> [(a,a)]
getPossibleAdjacent (x,y) = [(xa, ya) | xa <- [(x-1)..(x+1)], ya <- [(y-1)..(y+1)], not ((xa, ya) == (x,y))]
 
filterAdjacent :: (Integral a) => [(a, a)] -> [(a,a)] -> [(a,a)]
filterAdjacent x y = [ x' | x' <- x, x' `elem` y]

getLivingAdjacents :: (Integral a) => (a,a) -> [(a,a)] -> [(a,a)]
getLivingAdjacents = filterAdjacent . getPossibleAdjacent


main :: IO ()
main = putStrLn "hello"
