module Main where 
import Data.IORef
import Graphics.UI.GLUT
import Control.Monad

--unique :: [Int] -> [Int]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

-- This grabs all the cells that are adjacent cells to a supplied cell, making sure to not include that cell. 
-- Thank goodness tuples implement Eq. 
getPossibleAdjacent :: (Integral a) => (a,a) -> [(a,a)]
getPossibleAdjacent (x,y) = [(xa, ya) | xa <- [(x-1)..(x+1)], ya <- [(y-1)..(y+1)], not ((xa, ya) == (x,y))]

-- This simply filters down all the cells that return false in the supplied function. 
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



-- OpenGL stuff

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize 
  _window <- createWindow "Hello World"
  world <- newIORef [(3,1),(2,1), (1,1),(3,2),(2,3)]
  scaledWorld <- newIORef []
  displayCallback $= display scaledWorld
  idleCallback $= Just (idle world scaledWorld)
  mainLoop

scaleWorld = map ( \(x,y) -> ((fromIntegral x :: GLfloat), (fromIntegral y :: GLfloat)))

display :: IORef [(GLfloat,GLfloat)] -> DisplayCallback
display scaledWorld = do
  clear [ColorBuffer]
  x <- get scaledWorld
  renderPrimitive Points $ 
    mapM_ (\(x,y) -> vertex $ Vertex3 (x/256.0) (y/256.0) 0) x
  flush
 
idle :: IORef [(Integer, Integer)] -> IORef [(GLfloat, GLfloat)] -> IdleCallback
idle world scaledWorld = do
   w <- get world
   let newWorld = newSet w
   let sw = scaleWorld newWorld 
   writeIORef world newWorld
   writeIORef scaledWorld sw
   postRedisplay Nothing
