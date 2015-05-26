module Main where 

getAdjacent :: (Integral a) => (a,a) -> [(a,a)]
getAdjacent (x,y) = [(xa, ya) | xa <- [(x-1)..(x+1)], ya <- [(y-1)..(y+1)], not ((xa, ya) == (x,y))]
            
main :: IO ()
main = putStrLn "hello"
