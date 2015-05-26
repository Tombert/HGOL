module Main where 

getPossibleAdjacent :: (Integral a) => (a,a) -> [(a,a)]
getPossibleAdjacent (x,y) = [(xa, ya) | xa <- [(x-1)..(x+1)], ya <- [(y-1)..(y+1)], not ((xa, ya) == (x,y))]
 
filterAdjacent :: (Integral a) => ((a,a) -> [(a,a)] -> Bool) -> [(a, a)] -> [(a,a)] -> [(a,a)]
filterAdjacent f x y = [ x' | x' <- x, f x' y]

livingFilter = filterAdjacent elem
nonLivingFilter = filterAdjacent notElem

getLivingAdjacents = livingFilter . getPossibleAdjacent

getNonlivingAdjacents = nonLivingFilter . getPossibleAdjacent



main :: IO ()
main = putStrLn "hello"
