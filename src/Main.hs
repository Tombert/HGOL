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

-- Let's partially apply our utility functions. This will allow us to have a handy function to grab living and 
-- nonliving cells in the list grid. 
livingFilter = filterAdjacent elem
nonLivingFilter = filterAdjacent notElem

getLivingAdjacents = livingFilter . getPossibleAdjacent

livingAdjacentCount x xs = length $ getLivingAdjacents x xs

getNonlivingAdjacents = nonLivingFilter . getPossibleAdjacent

nonlivingAdjacentCount x xs = length $ getNonlivingAdjacents x xs

-- This function is where the new-world is calculated.  We apply conway's logic here by employing two 
-- list comprehensions.  This will tell whether or not a cell should be included in the new world.  Since 
-- the order of the cells doesn't really matter here, we can just concat the two comprehensions together. 
newSet x = remainingLiveCells ++ (unique spawnedDeadCells)
      where 
         remainingLiveCells = [x' | x' <- x, (livingAdjacentCount x' x) == 2 || (livingAdjacentCount x' x) == 3]
         spawnedDeadCells = [y' | x' <- x, y' <- (getNonlivingAdjacents x' x), (livingAdjacentCount y' x) == 3]



-- OpenGL stuff

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize 
  _window <- createWindow "Hello World"
  windowSize $= Size 640 480
  world <- newIORef [(3,1),(2,1), (1,1),(3,2),(2,3)]
  scaledWorld <- newIORef []
  displayCallback $= display scaledWorld
  idleCallback $= Just (idle world scaledWorld)
  reshapeCallback $= Just reshape 
  mainLoop

-- OpenGL likes your plain being from 0 to 1, but I ws using integers to store the values.  
-- This function just makes sure that the values are in the right format. 
scaleWorld = map ( \(x,y) -> ((fromIntegral x :: GLfloat), (fromIntegral y :: GLfloat)))

display :: IORef [(GLfloat,GLfloat)] -> DisplayCallback
display scaledWorld = do
  clear [ColorBuffer]
  x <- get scaledWorld
  renderPrimitive Points $ 
    mapM_ (\(x,y) -> vertex $ Vertex3 (x/100.0) (y/100.0) 0) x
  flush


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
 
idle :: IORef [(Integer, Integer)] -> IORef [(GLfloat, GLfloat)] -> IdleCallback
idle world scaledWorld = do
   w <- get world
   let newWorld = newSet w
   let sw = scaleWorld newWorld 
   writeIORef world newWorld
   writeIORef scaledWorld sw
   addTimerCallback (1000 `div` 1) (do postRedisplay Nothing)
   postRedisplay Nothing
