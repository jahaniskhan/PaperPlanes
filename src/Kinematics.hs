module Kinematics where

import Types 

-- Update velocity based on acceleration and time
updateVelocity :: Velocity -> Acceleration -> Double -> Velocity
updateVelocity (Velocity vx vy) (Acceleration ax ay) dt = 
    Velocity (vx + ax * dt) (vy + ay * dt) 

-- Update position based on velocity and time
updatePosition :: Position -> Velocity -> Double -> Position
updatePosition (Position x y) (Velocity vx vy) dt = 
    Position (x + vx * dt) (y + vy * dt) 

-- Update an entity's position and velocity based on its current state and time step
updateEntity :: Entity -> Double -> Entity
updateEntity (Entity pos vel acc color) dt = 
    let newPos = updatePosition pos vel dt
        newVel = updateVelocity vel acc dt
    in Entity newPos newVel acc color
