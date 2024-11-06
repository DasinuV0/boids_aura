module Main where

import Prelude hiding ( Left, Right )
import Graphics.Gloss -- ghci -fno-ghci-sandbox
import Graphics.Gloss.Data.ViewPort
import Linear.Metric
import Linear.V2
import Linear.Vector -- :set -package linear-1.23
import System.Random -- :set -package random

---------------------------------------------------
-- Visualisation framework  
---------------------------------------------------

type Position = V2 Float
type Velocity = V2 Float
type Index    = Int    -- do we need to identify them?
type TimeStep = Float

data Boid = Boid
  { idx :: Index,
    pos :: Position,
    vel :: Velocity
  }

instance Eq Boid where
  ballA == ballB = idx ballA == idx ballB

{-
Gloss is designed around the Model-View-Update principle. 
This principle requires the following inputs: a model for our system, a way to draw it 
on screen and a way to update the model throughout the simulation.
-}

type Model = [Boid]

main :: IO ()
main = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Boid 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> Picture 
    drawingFunc = pictures . (:) drawWalls . fmap drawBoid                    -- fmap?               usage of (:)? -- we append the result of the drawWalls function to the list of Picture before flattening the list of Picture to be drawn

    updateFunc :: ViewPort -> TimeStep -> Model -> Model
    updateFunc _ dt = newtonBounce dt

windowDisplay :: Display
windowDisplay = InWindow "Window" (1440, 900) (200, 800)
-- InWindow "Window" (1400, 900) (200, 800)

simulationRate :: Int
simulationRate = 10000


drawBoid :: Boid -> Picture
drawBoid (Boid _ (V2 x y) _) =                              -- the usage of _ as an empty value ? 
  translate x' y' $ colour (circleSolid $ toPixels dotSize)  -- translate? maps shaps into the coordinates?
  where
    x' = toPixels x
    y' = toPixels y
    colour = Color (withAlpha 0.8 blue)     -- this will needed to be changed and updated with every frame as well

toPixels :: Float -> Float
toPixels = (* 100.0)

dotSize :: Float
dotSize = 0.1


type Boids = [Boid]

newtonBounce :: Float -> Boids -> Boids
newtonBounce dt [boid@(Boid idx pos vel)] = [Boid idx pos' vel']
  where
    transVec = boundaryCondition boid
    vel' = transVec * vel
    pos' = pos + vel' ^* (dt*2)

---------------------------------------------------
-- Boundary 
---------------------------------------------------

drawWalls :: Picture
drawWalls = Circle (toPixels aLength)

boundaryCondition :: Boid -> V2 Float                            -- change this to the circular boundary condition
boundaryCondition (Boid _ (V2 x y) _)
  | (x' > aLength/2) && (y' > bLength/2) = V2 (-1) (-1)
  |  x' > aLength/2                       = V2 (-1)   1
  |  y' > bLength/2                      = V2   1  (-1)
  | otherwise                            = V2   1    1
   where
     x' = abs x + dotSize
     y' = abs y + dotSize

aLength, bLength :: Float
aLength = 14.0
bLength = 14.0



---------------------------------------------------
-- Several particles 
-- https://github.com/nwtgck/boid-haskell/blob/master/app/Main.hs
---------------------------------------------------

type Force        = V2 Float
type Acceleration = V2 Float

verletStep :: TimeStep -> Model -> Model
verletStep dt boids =
  let
     oldF     = calcForces boids
     oldA     = fmap (^/ m) oldF
     newPos   = updatePositions dt boids oldA
     newF     = calcForces newPos
     newA     = fmap (^/ m) newF
     addedF   = oldA ^+^ newA
     newParts = updateVelocities dt newPos addedF
  in newParts


updatePosition :: TimeStep -> Boid -> Acceleration -> Boid
updatePosition dt (Boid idx pos vel) acc = Boid idx newPos vel
  where
   newPos  = pos ^+^ velPart ^+^ accPart
   velPart = vel ^* dt
   accPart = acc ^* (0.5 * dt**2)


updateVelocity :: TimeStep -> Boid -> Acceleration -> Boid
updateVelocity dt boid acc = Boid idx pos vel'
  where
    (Boid idx pos vel) = boid
    transVec = boundaryCondition boid
    vel' = transVec * (vel + (0.5 * dt) *^ acc)

{-
The above two equations are used to update the Position and Velocity of a single particle, respectively. 
To make these functions applicable for multiple Particles, we can use zipWith:
-}


updatePositions, updateVelocities :: TimeStep -> [Boid] -> [Force] -> [Boid]
updatePositions  dt = zipWith (updatePosition dt)
updateVelocities dt = zipWith (updateVelocity dt)


-- Need to use the rules of seperation to encode calcForces

calcForces 
---------------------------------------------------
-- The rules of interaction 
---------------------------------------------------

-- separation 

-- alignment 
-- First, we'll make a function that takes an agent and returns a velocity vector.

velVector :: Boid -> V2 Float
velVector (Boid idx pos vel) = vel

-- let neighborCount = 0 

-- cohesion 

{- How do I encode neighborhood? I should just modify somehow the updating funtion. For example,
for cohesion neighborhood would determine the weighted affect of other boids. 
-} 
