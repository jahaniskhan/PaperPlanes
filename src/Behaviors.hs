module Behaviors where 

import Types
import Kinematics
import Prelude hiding (cos, sin)
import qualified Prelude as P
import Graphics.Gloss.Data.Color

hover :: Entity -> Entity
hover (Entity pos _ _ color) = Entity pos (Velocity 0 0 0) (Acceleration 0 0 0) color

moveLinearly :: Velocity -> Entity -> Entity 
moveLinearly (Velocity vx vy vz) (Entity pos _ _ color) = Entity pos (Velocity vx vy vz) (Acceleration 0 0 0) color

accelerate :: Acceleration -> Entity -> Entity 
accelerate acc (Entity pos vel _ color) = Entity pos vel acc color

turn :: Float -> Entity -> Entity
turn angle (Entity pos (Velocity vx vy vz) acc color) = 
    Entity pos (Velocity (vx * P.cos (realToFrac angle) - vy * P.sin (realToFrac angle)) 
                         (vx * P.sin (realToFrac angle) + vy * P.cos (realToFrac angle)) vz) acc color

chase :: Entity -> Entity -> Entity 
chase (Entity (Position dx dy dz) (Velocity vx vy vz) _ droneColor) (Entity (Position tx ty tz) _ _ _ ) = 
    let 
        -- calculate direction vector
        directionX = tx - dx
        directionY = ty - dy
        directionZ = tz - dz

        -- calculate distancre to target
        distance  = sqrt (directionX^2 + directionY^2 + directionZ^2)

        -- nomralize direction vector
        normDirectionX = directionX / distance
        normDirectionY = directionY / distance
        normDirectionZ = directionZ / distance

        -- adjust speed on distance
        chaseSpeed = min 1 (distance / 10)

        -- adjust velocity gradually
        newVelX = vx + normDirectionX * chaseSpeed * 0.1
        newVelY = vy + normDirectionY * chaseSpeed * 0.1
        newVelZ = vz + normDirectionZ * chaseSpeed * 0.1
    in Entity (Position dx dy dz) (Velocity newVelX newVelY newVelZ) (Acceleration 0 0 0) droneColor


-- because position velocity and color are defined
initialPosition :: Position 
initialPosition = Position 0 0 0

initialVelocity :: Velocity
initialVelocity = Velocity 0 0 0

initialAcceleration :: Acceleration
initialAcceleration = Acceleration 0 0 0



initialErraticTarget :: Entity 
initialErraticTarget = Entity initialPosition initialVelocity initialAcceleration blue
-- this 

erraticMove :: Float -> Entity -> Entity 
erraticMove time (Entity pos (Velocity vx vy vz) acc color) = 
    let 
        newVelX = vx + (P.sin (realToFrac time)) -- sinsoidal variation because our target is lazy
        newVelY = vy + (P.cos (realToFrac time))
        newVelZ  = vz
    in Entity pos (Velocity newVelX newVelY newVelZ) acc color


