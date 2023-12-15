module Visualization where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Simulate
import Types
import Behaviors (initialPosition, initialVelocity, initialAcceleration, initialErraticTarget)
import qualified Simulate as S

window :: Display
window = InWindow "Drone Simulation" (640, 480) (10, 10)

background :: Color
background = white


-- Convert an entity to a picture
-- we need a label to test if it is a drone or a target
drawEntityWithLabel :: Entity -> Picture
drawEntityWithLabel (Entity (Position x y) _ _ entityzcolor) = 
    let entityPic = translate (realToFrac x) (realToFrac y) $ color entityzcolor $ circleSolid 10
        label = if entityzcolor == red then "Drone" else "Target"
        textPic = translate (realToFrac x + 10) (realToFrac y) $ scale 0.1 0.1 $color black $ text label
    in pictures [entityPic, textPic]



-- Combine all the pictures into one
render :: [Entity] -> Picture 
render entities = pictures $ map drawEntityWithLabel entities

--function to generate path 

runSimulationForTime :: Float -> IO [Entity]
runSimulationForTime time = do
    let drone = Entity initialPosition initialVelocity initialAcceleration red
    let seed = floor time -- time seed for random
    (updatedDrone, updatedTarget) <- S.simulate drone initialErraticTarget seed time 10.0
    return [updatedDrone, updatedTarget]


type WorldState = [Entity]

initialState :: WorldState
initialState = [S.initialDrone, S.initialTarget]

--update worldstate
updateWorld :: ViewPort -> Float -> WorldState -> IO WorldState
updateWorld _ time _ = runSimulationForTime time
   
-- Main visualization function to display the simulation
visualizeSimulation :: WorldState -> IO Picture
visualizeSimulation entities = return $ render entities

-- Main function to run the visualization
animateSimulation :: IO ()
animateSimulation = 
    simulateIO window background fps initialState visualizeSimulation updateWorld

fps :: Int
fps = 60

main :: IO ()
main = animateSimulation
