module Simulate 
( simulate,
initialDrone, 
initialTarget,
generateTargetPath, someConstantVelocity
) where

import Types
import Kinematics
import Behaviors 
import System.Random
import Data.List
import Graphics.Gloss.Data.Color
import System.Random(randomRs, newStdGen)

-- to run simulateIO, we need to pass in an initial drone, target, and target path

initialDronePosition :: Position 
initialDronePosition = Position 320 240 0

initialDroneVelocity :: Velocity
initialDroneVelocity  = Velocity 0 0 0

initialDroneAcceleration :: Acceleration
initialDroneAcceleration = Acceleration 0 0 0

initialDrone :: Entity
initialDrone = Entity initialDronePosition initialDroneVelocity initialDroneAcceleration red

--defining target placeholder for scope patch
initialTargetPosition :: Position
initialTargetPosition = Position 10 10 10 

initialTargetVelocity :: Velocity
initialTargetVelocity = Velocity 1 1 1

initialTarget :: Entity
initialTarget = Entity initialTargetPosition initialTargetVelocity (Acceleration 0 0 0) blue


-- generate Gaussian noise with a given mean and standard deviation 
generateNoise :: Float -> Float -> IO Float
generateNoise mean stdDev = do
    val <- randomIO :: IO Float
    return $ mean + stdDev * val

-- add noise to an entity's position
addNoiseToEntity :: Entity -> Float -> Entity
addNoiseToEntity (Entity(Position x y z) vel acc color) noise = 
    Entity (Position (x + realToFrac noise) (y + realToFrac noise) (z + realToFrac noise)) vel acc color

catmullRom :: Position -> Position -> Position -> Position -> Float -> Position
catmullRom (Position x0 y0 z0) (Position x1 y1 z1) (Position x2 y2 z2) (Position x3 y3 z3) t =
    Position x y z
    where 
        t' = realToFrac t 
        x = 0.5 * ((2 * x1) + (-x0 + x2) * t' + (2* x0 - 5*x1 + 4*x2 - x3) * t'^2 + (-x0 + 3*x1 - 3*x2 + x3) * t'^3)
        y = 0.5 * ((2 * y1) + (-y0 + y2) * t' + (2* y0 - 5*y1 + 4*y2 - y3) * t'^2 + (-y0 + 3*y1 - 3*y2 + y3) * t'^3)
        z = 0.5 * ((2 * z1) + (-z0 + z2) * t' + (2* z0 - 5*z1 + 4*z2 - z3) * t'^2 + (-z0 + 3*z1 - 3*z2 + z3) * t'^3)

-- generate a segment of a path using Catmull-Rom splines
generateSegment :: (Position, Position, Position, Position) ->[Position]
generateSegment (p0, p1, p2,p3) = 
    [ catmullRom p0 p1 p2 p3 t | t <- [0, 0.1 .. 1] ]
-- Generate random control points
randomControlPoints :: Int -> IO [Position]
randomControlPoints n = do
    gen <- newStdGen
    let xs = take n $ randomRs (0, 50) gen
        ys = take n $ randomRs (0, 50) gen
        zs = take n $ randomRs (0, 50) gen
    return $ zipWith3 Position xs ys zs

-- update generate target path to accept a random seed
generateTargetPath :: Int -> IO [Position]
generateTargetPath seed = do 
    controlPoints <- randomControlPoints 5 -- generate 5 random control points
    return $ concatMap generateSegment (zip4 controlPoints(drop 1 controlPoints) (drop 2 controlPoints) (drop 3 controlPoints))
   


updateTarget :: [Position] -> Float -> Entity -> Entity
updateTarget path time maxTime target = 
    -- we need to update target's postion based o nthe time and the path 
    case path of 
        [] -> target -- handle empty path
        _ -> 
            let pathLength = fromIntegral (length path)
                t = time / (realToFrac maxTime)
                index = floor(t * pathLength)
                newPosition = path !! (index `mod` length path)
            in target { entityPosition = newPosition }

simulate :: Entity -> Entity -> Int -> Float -> Float -> IO (Entity, Entity)
simulate drone target seed currentTime maxTime 
    | currentTime >= maxTime = do -- handles when the path is empty
    putStrLn "Simulation complete"
    return (drone, target)
    | otherwise = do
        putStrLn $ "current time" ++ show currentTime
        putStrLn $ "current drone position" ++ show (entityPosition drone)
       
        -- generate a path for the target
        path <- generateTargetPath seed

        -- we want to make sure path isnt empty
        if null path
            then return (drone, target)
        else do
            let droneNoise = 0.05 
                targetNoise = 0.05
            let noisyDrone = addNoiseToEntity drone droneNoise
            let currentTargetPosition = head path
            let noisyTarget = addNoiseToEntity target targetNoise
            let updatedTarget = addNoiseToEntity (target {entityPosition = currentTargetPosition}) targetNoise
            let chasingDrone = chase noisyDrone noisyTarget
            let updatedDrone = updateEntity chasingDrone (realToFrac dt)
            putStrLn $ "updated drone position" ++ show (entityPosition updatedDrone)
            let updatedTarget' = updateTarget path (realToFrac currentTime) updatedTarget
            putStrLn $ "updated drone position" ++ show (entityPosition updatedTarget')

            --recursive call with updated entities and incremented seed
            simulate updatedDrone updatedTarget' (seed + 1) (currentTime + dt) maxTime
         

-- time step
dt :: Float
dt = 0.1

-- constant velocity for target
someConstantVelocity :: Velocity
someConstantVelocity = Velocity 1 1 1

