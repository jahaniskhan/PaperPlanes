-- Types.hs
module Types where
import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- | represents a position in R3
data Position = Position { xPos :: Double
                         , yPos :: Double
                         } deriving (Show, Eq)
-- eq means that we can compare two positions for equality
-- deriving meaning that we can derive the show and eq instances for Position

-- |represents a velocity in R3
data Velocity = Velocity { xVel :: Double
                         , yVel :: Double
                         } deriving (Show, Eq)

-- |reperesnts acceleration
data Acceleration = Acceleration { xAcc :: Double
                                 , yAcc :: Double
                                 } deriving (Show, Eq)

data Entity  = Entity{
    entityPosition :: Position,
    entityVelocity :: Velocity,
    entityAcceleration :: Acceleration,
    entityzcolor :: Color
} deriving (Show, Eq)