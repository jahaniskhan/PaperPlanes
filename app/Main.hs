module Main where

import qualified MyLib (someFunc)
import qualified Simulate as S
import Types
import Kinematics
import Behaviors
import Simulate
import Data.List(cycle)
import Visualization(animateSimulation)
import Graphics.Gloss.Data.Color(red,blue,green, Color)

-- | The main entry point.
initializeEntity :: Position -> Velocity -> Maybe Color-> Entity 
initializeEntity pos vel mColor = Entity pos vel (Acceleration 1 1) (fromMaybe defaultColor mColor)

defaultColor :: Color
defaultColor = green

fromMaybe :: a -> Maybe a -> a
fromMaybe defVal Nothing = defVal
fromMaybe _ (Just val) = val

dt :: Double
dt = 0.1
maxSimulationTime :: Float
maxSimulationTime = 10

someConstantVelocity :: Velocity
someConstantVelocity = Velocity 1 1 

updateTarget :: [Position] -> Float -> Entity -> Entity
updateTarget path time target = 
  if null path
    then target -- handle empty path
    else
      let pathLength = length path
          index = floor(time * fromIntegral pathLength / maxSimulationTime)
          newPosition = path !! index
      in target { entityPosition = newPosition }




runSimulate :: Entity -> Entity -> [Position] -> Float -> IO ()
runSimulate drone target path time = do
    putStrLn $ "Simulation Time" ++ show time
    putStrLn $ "Drone Position" ++ show (entityPosition drone)
    putStrLn $ "Target Position" ++ show(entityPosition target)
    putStrLn $ "Target Velocity" ++ show(entityVelocity target)

    -- updated based on time
    let updatedTarget = updateTarget path (realToFrac time) target
    let chasingDrone = chase drone updatedTarget
    let updateDrone = updateEntity chasingDrone (realToFrac dt)

    putStrLn $ "Updated Drone Position" ++ show (entityPosition updateDrone)
    putStrLn $ "Updated Drone Velocity" ++ show (entityVelocity updateDrone)
    putStrLn $ "Updated Target Position" ++ show (entityPosition updatedTarget)
    putStrLn $ "Updated Target Velocity" ++ show (entityVelocity updatedTarget)
    
    if time >= maxSimulationTime 
      then putStrLn "Simulation Complete"
      else runSimulate updateDrone updatedTarget path (time + realToFrac dt)

main :: IO ()
main = do 
  MyLib.someFunc
  animateSimulation

  let initialDrone = initializeEntity (Position 10 10) (Velocity 0 0) (Just red)
  putStrLn $ "Initial Drone" ++ show initialDrone
  let initialTarget = initializeEntity (Position 100 100) (Velocity 0 0) (Just blue)
  --initialized conditions for drone and target
  generatedPath <- generateTargetPath 42
  let targetPath = cycle generatedPath

  -- generated a path for target to follow

  runSimulate initialDrone initialTarget targetPath 0 
  -- running simulation

 

