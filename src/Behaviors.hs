module Behaviors where 

import Types
import Kinematics
import Prelude hiding (cos, sin)
import qualified Prelude as P
import Graphics.Gloss.Data.Color
import Debug.Trace (trace)

hover :: Entity -> Entity
hover (Entity pos _ _ color) = Entity pos (Velocity 0 0) (Acceleration 0 0) color

moveLinearly :: Velocity -> Entity -> Entity 
moveLinearly (Velocity vx vy) (Entity pos _ _ color) = Entity pos (Velocity vx vy) (Acceleration 0 0) color

accelerate :: Acceleration -> Entity -> Entity 
accelerate acc (Entity pos vel _ color) = Entity pos vel acc color

turn :: Float -> Entity -> Entity
turn angle (Entity pos (Velocity vx vy) acc color) = 
    Entity pos (Velocity (vx * P.cos (realToFrac angle) - vy * P.sin (realToFrac angle)) 
                         (vx * P.sin (realToFrac angle) + vy * P.cos (realToFrac angle))) acc color

chase :: Entity -> Entity -> Entity 
chase drone@(Entity (Position dx dy) (Velocity vx vy) _ droneColor) target@(Entity (Position tx ty) _ _ _) = 
    let
        directionX = tx - dx
        directionY = ty - dy

        distance = sqrt (directionX^2 + directionY^2)
        (normDirectionX, normDirectionY) = if distance == 0
            then (0, 0)
            else (directionX / distance, directionY / distance)
        chaseSpeed = min 1 (distance / 10)
        newVelX = vx + normDirectionX * chaseSpeed * 0.1
        newVelY = vy + normDirectionY * chaseSpeed * 0.1

    in Entity (Position dx dy) (Velocity newVelX newVelY) (Acceleration 0 0) droneColor

-- because position velocity and color are defined
initialPosition :: Position 
initialPosition = Position 0 0

initialVelocity :: Velocity
initialVelocity = Velocity 0 0

initialAcceleration :: Acceleration
initialAcceleration = Acceleration 0 0



initialErraticTarget :: Entity 
initialErraticTarget = Entity initialPosition initialVelocity initialAcceleration blue
-- this 

erraticMove :: Float -> Entity -> Entity 
erraticMove time (Entity pos (Velocity vx vy) acc color) = 
    let 
        newVelX = vx + (P.sin (realToFrac time)) -- sinsoidal variation because our target is lazy
        newVelY = vy + (P.cos (realToFrac time))
    in Entity pos (Velocity newVelX newVelY) acc color


