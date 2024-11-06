module Main where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Linear.V2
import Linear.Metric
import Linear.Vector
import System.Random

{-
Just do
cabal update
cabal build
cabal run
-}

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
main = do
  boids <- randomBoids 100
  simulate windowDisplay white simulationRate boids drawingFunc updateFunc

---------------------------------------------------
-- Random Boid Generation
---------------------------------------------------

randomBoid :: Index -> IO Boid
randomBoid idx = do
  x <- randomRIO (-7, 7) 
  y <- randomRIO (-7, 7)
  vx <- randomRIO (-1, 1)
  vy <- randomRIO (-1, 1)
  return $ Boid idx (V2 x y) (V2 vx vy)

randomBoids :: Int -> IO [Boid]
randomBoids n = mapM randomBoid [1..n]

windowDisplay :: Display
windowDisplay = InWindow "Window" (1440, 900) (200, 800)
-- InWindow "Window" (1400, 900) (200, 800)

simulationRate :: Int
simulationRate = 144

---------------------------------------------------
-- Drawing Functions
---------------------------------------------------

drawingFunc :: Model -> Picture 
drawingFunc = pictures . (:) drawWalls . fmap drawBoid                    -- fmap?               usage of (:)? -- we append the result of the drawWalls function to the list of Picture before flattening the list of Picture to be drawn

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
dotSize = 0.05

---------------------------------------------------
-- Update Functions
---------------------------------------------------

type Boids = [Boid]

updateFunc :: ViewPort -> TimeStep -> Model -> Model
updateFunc _ dt = newtonBounce dt

maxSpeed :: Float
maxSpeed = 2.0  -- Maximum speed for boids

updateBoid :: [Boid] -> Float -> Boid -> Boid
updateBoid boids dt boid@(Boid idx pos vel) = Boid idx pos' vel'
  where
    -- Forces
    sepForce = separationForce boid boids ^* 0.05
    alignForce = alignmentForce boid boids ^* 1.25
    cohForce = cohesionForce boid boids ^* 0.2

    -- Apply friction and scale the velocity
    velWithForces = vel + sepForce + alignForce + cohForce
    vel' = clampSpeed $ velWithForces ^* 1.003
    
    -- Update position with time step
    pos' = boundaryCondition (pos + vel' ^* dt)

-- Function to clamp the speed of a velocity vector
clampSpeed :: Velocity -> Velocity
clampSpeed v = if speed > maxSpeed
                then normalize v ^* maxSpeed
                else v
  where
    speed = norm v

-- Apply update to each boid
newtonBounce :: Float -> Model -> Model
newtonBounce dt boids = map (updateBoid boids dt) boids

---------------------------------------------------
-- Boundary 
---------------------------------------------------

drawWalls :: Picture
drawWalls = Circle (toPixels aLength)

boundaryRadius :: Float
boundaryRadius = 7.5  -- Radius of the circular boundary

boundaryCondition :: Position -> Position
boundaryCondition (V2 x y) = V2 wrappedX wrappedY
  where
    -- Wrap around the X coordinate
    wrappedX = if x > boundaryRadius then -boundaryRadius
               else if x < -boundaryRadius then boundaryRadius
               else x
    -- Wrap around the Y coordinate
    wrappedY = if y > boundaryRadius then -boundaryRadius
               else if y < -boundaryRadius then boundaryRadius
               else y

aLength, bLength :: Float
aLength = 14.0
bLength = 14.0

---------------------------------------------------
-- Several particles and the rules of interaction 
-- https://github.com/nwtgck/boid-haskell/blob/master/app/Main.hs
---------------------------------------------------

type Force        = V2 Float
type Acceleration = V2 Float

distanceEU :: Position -> Position -> Float
distanceEU p1 p2 = norm (p1 - p2)

-- separation 

separationDistance :: Float
separationDistance = 0.2  -- Minimum distance to maintain from other boids

-- Compute the separation force for a single boid, based on nearby boids
separationForce :: Boid -> [Boid] -> Force
separationForce boid boids = 
    foldr (\otherBoid acc -> 
        if boid /= otherBoid && distanceEU (pos boid) (pos otherBoid) < separationDistance 
        then acc + repelForce boid otherBoid
        else acc
    ) (V2 0 0) boids
  where
    -- Helper function to calculate repulsion force between two boids
    repelForce :: Boid -> Boid -> Force
    repelForce (Boid _ posA _) (Boid _ posB _) =
        let direction = posA - posB
            dist = max 0.01 (norm direction) -- Avoid division by zero
        in if dist < separationDistance 
           then (normalize direction) ^/ dist  -- The closer, the stronger the force
           else V2 0 0

-- alignment 

alignmentDistance :: Float
alignmentDistance = 1.5  -- Distance within which boids align their velocities

alignmentForce :: Boid -> [Boid] -> Force
alignmentForce boid boids =
    if count > 0
    then (avgVel ^-^ vel boid) ^* 0.05  -- Scale alignment effect
    else V2 0 0
  where
    -- Gather velocities of nearby boids within alignmentDistance
    (totalVel, count) = foldr (\otherBoid (sumVel, n) ->
        if boid /= otherBoid && distanceEU (pos boid) (pos otherBoid) < alignmentDistance
        then (sumVel + vel otherBoid, n + 1)
        else (sumVel, n)
      ) (V2 0 0, 0) boids

    -- Calculate the average velocity
    avgVel = totalVel ^/ fromIntegral count

-- cohesion 

cohesionDistance :: Float
cohesionDistance = 1.2  -- Distance within which boids move towards average position

cohesionForce :: Boid -> [Boid] -> Force
cohesionForce boid boids =
    if count > 0
    then (avgPos ^-^ pos boid) ^* 0.05  -- Scale cohesion effect
    else V2 0 0
  where
    -- Gather positions of nearby boids within cohesionDistance
    (totalPos, count) = foldr (\otherBoid (sumPos, n) ->
        if boid /= otherBoid && distanceEU (pos boid) (pos otherBoid) < cohesionDistance
        then (sumPos + pos otherBoid, n + 1)
        else (sumPos, n)
      ) (V2 0 0, 0) boids

    -- Calculate the average position
    avgPos = totalPos ^/ fromIntegral count

{- How do I encode neighborhood? I should just modify somehow the updating funtion. For example,
for cohesion neighborhood would determine the weighted affect of other boids. 
-} 