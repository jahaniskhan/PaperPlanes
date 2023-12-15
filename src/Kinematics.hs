module Kinematics where

import Types 

-- Update velocity based on acceleration and time
updateVelocity :: Velocity -> Acceleration -> Double -> Velocity
updateVelocity (Velocity vx vy vz) (Acceleration ax ay az) dt = 
    Velocity (vx + ax * dt) (vy + ay * dt) (vz + az * dt)

-- Update position based on velocity and time
updatePosition :: Position -> Velocity -> Double -> Position
updatePosition (Position x y z) (Velocity vx vy vz) dt = 
    Position (x + vx * dt) (y + vy * dt) (z + vz * dt)

-- Update an entity's position and velocity based on its current state and time step
updateEntity :: Entity -> Double -> Entity
updateEntity (Entity pos vel acc color) dt = 
    Entity newPos newVel acc color
    where
        newPos = updatePosition pos newVel dt
        newVel = updateVelocity vel acc dt
