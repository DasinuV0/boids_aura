{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Linear.V2
import Linear.Metric
import Linear.Vector
import System.Random
import GHC.Exts (SpecConstrAnnotation(ForceSpecConstr))

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
type Index    = Int    
type TimeStep = Float

data Boid = Boid     
  { idx :: Index,
    pos :: Position,
    vel :: Velocity,
    col :: Color
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
  boids <- randomBoids 250
  simulate windowDisplay black simulationRate boids drawingFunc updateFunc

---------------------------------------------------
-- Random Boid Generation
---------------------------------------------------

randomBoid :: Index -> IO Boid     
randomBoid idx = do
  x <- randomRIO (-7, 7)
  y <- randomRIO (-7, 7)
  vx <- randomRIO (-1, 1)
  vy <- randomRIO (-1, 1)
  let defaultColor = makeColor 1 0 0 1
  return $ Boid idx (V2 x y) (V2 vx vy) defaultColor

randomBoids :: Int -> IO [Boid]
randomBoids n = mapM randomBoid [1..n]

windowDisplay :: Display
windowDisplay = InWindow "Window" (1440, 900) (200, 800) 

simulationRate :: Int
simulationRate = 144

---------------------------------------------------
-- Drawing Functions
---------------------------------------------------

drawingFunc :: Model -> Picture
drawingFunc = pictures . (:) drawWalls . fmap drawBoid                    

drawBoid :: Boid -> Picture
drawBoid (Boid _ (V2 x y) velocity _) =                              
  translate x' y' $ colour (circleSolid $ toPixels dotSize)  
  where
    x' = toPixels x
    y' = toPixels y
    colour = Color (velocityToColor velocity)    

toPixels :: Float -> Float
toPixels = (* 100.0)

dotSize :: Float
dotSize = 0.03

---------------------------------------------------
-- Update Functions
---------------------------------------------------

type Boids = [Boid]

updateFunc :: ViewPort -> TimeStep -> Model -> Model
updateFunc _ dt boids = 
  let quadTree = buildQuadTree (-aLength / 2, aLength / 2, -bLength / 2, bLength / 2) boids 0
      updatedBoids = newtonBounce dt boids  -- Update positions and velocities first
  in map (resolveCollisions quadTree dt) updatedBoids  -- Resolve collisions after updating

resolveCollisions :: QuadTree -> TimeStep -> Boid -> Boid
resolveCollisions quadTree dt boid =
  let collisions = detectCollisions quadTree boid 0  -- Get list of boids that are in collision range
  in foldr adjustBoid boid collisions  -- Apply adjustments for each boid in the collision list
  where
    adjustBoid :: Boid -> Boid -> Boid
    adjustBoid otherBoid boid =
      let distance = norm (pos boid - pos otherBoid)
          repulsion = if distance < protectedRange
                      then normalize (pos boid - pos otherBoid) ^* (avoidfactor / distance)
                      else V2 0 0
          dampenedVel = vel boid + repulsion
      in boid { vel = clampSpeed dampenedVel }

maxSpeed :: Float
maxSpeed = 3.5  

minSpeed :: Float
minSpeed = 2.5 

updateBoid :: [Boid] -> Float -> Boid -> Boid
updateBoid boids dt boid@(Boid idx pos vel col) = Boid idx pos' vel' col'
  where
    -- Forces
    sepForce = separationForce boid boids 
    alignForce = alignmentForce boid boids ^* 0.1
    cohForce = cohesionForce boid boids ^* 0.028

    -- Update position with time step
    pos' = pos + vel' ^* dt 

    -- Scale the velocity
    velWithForces = vel + sepForce + alignForce + cohForce
    vel'' = clampSpeed' $ clampSpeed $ velWithForces ^* 1.003
    vel' = boundaryCondition (Boid idx pos vel'' col)

    col' = velocityToColor vel'


-- Function to clamp the speed of a velocity vector
clampSpeed :: Velocity -> Velocity
clampSpeed v = if speed > maxSpeed
                then normalize v ^* maxSpeed
                else v
  where
    speed = norm v

clampSpeed' :: Velocity -> Velocity
clampSpeed' v = if speed < minSpeed
                then normalize v ^* minSpeed
                else v
  where
    speed = norm v

velocityToColor:: Velocity -> Color
velocityToColor velocity = 
  let hue = (maxSpeed - (norm velocity))/(maxSpeed - minSpeed)
      (r, g, b) = if hue <= 0.005 then (1, 0.8-hue, 0.2) else 
                  if hue >= 0.1 && hue <= 0.4 then (1, 0.3+0.5*hue, 0.1) else
                  if hue >= 0.4 && hue <= 0.7 then (1, 0.3+0.5*hue, 0.1) else
                  if hue >= 0.7 && hue <= 0.95 then (1, 0.3+0.5*hue, 0) else (1, 0.4, 0)
  in makeColor r g b 1.0

-- Apply update to each boid
newtonBounce :: Float -> Model -> Model
newtonBounce dt boids = map (updateBoid boids dt) boids

---------------------------------------------------
-- Boundary 
---------------------------------------------------

drawWalls :: Picture
drawWalls = lineLoop $ rectanglePath (toPixels aLength) (toPixels bLength)

boundaryCondition :: Boid -> V2 Float              -- Forces boids to stay with in certain distance from set margins
boundaryCondition (Boid _ (V2 x y) (V2 x'' y'') _)
  | (x' > rightmargin) && (y' > topmargin) = V2 (x'' - turnfactor) (y'' - turnfactor)
  |  x' > rightmargin                      = V2 (x'' - turnfactor) y''
  |  y' > topmargin                      = V2 x'' (y'' - turnfactor)
  |  y' < bottommargin                      = V2 x'' (y'' + turnfactor) 
  |  x' < leftmargin                      = V2 (x'' + turnfactor) y''
  |  x' < leftmargin && (y' < bottommargin)  = V2 (x'' + turnfactor) (y'' + turnfactor)
  | otherwise                            = V2   x''    y''
   where
     x' | x >= 0 = x + dotSize
        | x < 0 = x - dotSize  
     y' | y >= 0 =  y + dotSize 
        | y < 0 = y - dotSize 

aLength, bLength :: Float
aLength = 8
bLength = 4

leftmargin :: Float
leftmargin = - (aLength / 2)
rightmargin :: Float
rightmargin = aLength/2
topmargin :: Float
topmargin = bLength/2
bottommargin :: Float
bottommargin = - (bLength / 2)

---------------------------------------------------
-- The rules of interaction 
---------------------------------------------------

type Force        = V2 Float
type Acceleration = V2 Float

distanceEU :: Position -> Position -> Float
distanceEU p1 p2 = norm (p1 - p2)

squaredDistance :: Position -> Position -> Float
squaredDistance p1 p2 = sqrt $ distanceEU p1 p2

-- Separation 

clampForce :: Float -> Force -> Force
clampForce maxForce f =
  if norm f > maxForce
  then normalize f ^* maxForce
  else f
  
-- Compute the separation force for a single boid, based on nearby boids
separationForce :: Boid -> [Boid] -> Force
separationForce boid others =
  foldr (\other acc ->
      if boid /= other && distanceEU (pos boid) (pos other) < protectedRange
      then acc + smoothRepulsion (pos boid) (pos other)
      else acc
    ) (V2 0 0) others
  where
    smoothRepulsion :: Position -> Position -> Force
    smoothRepulsion p1 p2 = clampForce maxRepulsion $ normalize (p1 - p2) ^* (avoidfactor / (d * d + 0.01))
      where
        d = distanceEU p1 p2
        maxRepulsion = 1.5



-- Alignment 

alignmentForce :: Boid -> [Boid] -> Force
alignmentForce boid boids =
    if count > 0
    then (avgVel ^-^ vel boid) ^* scalealignmen  -- Scale alignment effect
    else V2 0 0
  where
    -- Gather velocities of nearby boids within alignmentDistance
    (totalVel, count) = foldr (\otherBoid (sumVel, n) ->
        if boid /= otherBoid && distanceEU (pos boid) (pos otherBoid) < visualRange
        then (sumVel + vel otherBoid, n + 1)
        else (sumVel, n)
      ) (V2 0 0, 0) boids

    -- Calculate the average velocity
    avgVel = totalVel ^/ fromIntegral count

-- Cohesion 

cohesionForce :: Boid -> [Boid] -> Force
cohesionForce boid boids =
    if count > 0
    then (avgPos ^-^ pos boid) ^* centeringfactor 
    else V2 0 0
  where
    -- Gather positions of nearby boids within visual range
    (totalPos, count) = foldr (\otherBoid (sumPos, n) ->
        if boid /= otherBoid && distanceEU (pos boid) (pos otherBoid) < visualRange
        then (sumPos + pos otherBoid, n + 1)
        else (sumPos, n)
      ) (V2 0 0, 0) boids

    -- Calculate the average position
    avgPos = totalPos ^/ fromIntegral count

--------------------------------------
-- Collision detection
--------------------------------------

instance Show Boid where
  show (Boid idx (V2 x y) (V2 vx vy) _) =
    "Boid " ++ show idx ++ " at (" ++ show x ++ ", " ++ show y ++ ") with velocity (" ++ show vx ++ ", " ++ show vy ++ ")"

data QuadTree = Node
  { bounds :: (Float, Float, Float, Float), -- (xMin, xMax, yMin, yMax)
    points :: [Boid],                      -- Boids in this node
    nw     :: Maybe QuadTree,              -- North-West quadrant
    ne     :: Maybe QuadTree,              -- North-East quadrant
    sw     :: Maybe QuadTree,              -- South-West quadrant
    se     :: Maybe QuadTree               -- South-East quadrant
  }
  | Empty                                   -- Empty region
  deriving (Show, Eq)

maxPoints :: Int
maxPoints = 20 -- max boids in a node before splitting

maxDepth :: Int
maxDepth = 7 -- max recursion depth

buildQuadTree :: (Float, Float, Float, Float) -> [Boid] -> Int -> QuadTree
buildQuadTree bounds@(xMin, xMax, yMin, yMax) boids depth
  | depth > maxDepth = Empty  -- Stop recursion if max depth is reached
  | null boids = Empty
  | length boids <= maxPoints = Node bounds boids Nothing Nothing Nothing Nothing
  | otherwise = Node bounds [] (Just nw') (Just ne') (Just sw') (Just se')
  where
    -- Calculate midpoints
    xMid = (xMin + xMax) / 2
    yMid = (yMin + yMax) / 2

    -- Partition boids into quadrants
    (nwBoids, neBoids, swBoids, seBoids) = partitionBoids (xMid, yMid) boids

    -- Recursively build child quad trees, incrementing depth
    nw' = buildQuadTree (xMin, xMid, yMid, yMax) nwBoids (depth + 1)
    ne' = buildQuadTree (xMid, xMax, yMid, yMax) neBoids (depth + 1)
    sw' = buildQuadTree (xMin, xMid, yMin, yMid) swBoids (depth + 1)
    se' = buildQuadTree (xMid, xMax, yMin, yMid) seBoids (depth + 1)

partitionBoids :: (Float, Float) -> [Boid] -> ([Boid], [Boid], [Boid], [Boid])
partitionBoids (xMid, yMid) boids = foldr assign ([], [], [], []) boids
  where
    assign :: Boid -> ([Boid], [Boid], [Boid], [Boid]) -> ([Boid], [Boid], [Boid], [Boid])
    assign boid (nw, ne, sw, se) =
      case pos boid of
        V2 x y
          | x < xMid && y >= yMid -> (boid : nw, ne, sw, se)  -- NW
          | x >= xMid && y >= yMid -> (nw, boid : ne, sw, se)  -- NE
          | x < xMid && y < yMid   -> (nw, ne, boid : sw, se)  -- SW
          | otherwise              -> (nw, ne, sw, boid : se)  -- SE

queryQuadTree :: QuadTree -> (Float, Float, Float, Float) -> Int -> [Boid]
queryQuadTree Empty _ _ = []
queryQuadTree (Node bounds points nw ne sw se) searchBounds depth
  | depth > maxDepth = []  -- Stop querying if depth exceeds maxDepth
  | not (overlap bounds searchBounds) = [] -- No overlap, return empty
  | otherwise = points ++ concatMap (\child -> maybe [] (\c -> queryQuadTree c searchBounds (depth + 1)) child) [nw, ne, sw, se]

-- Check if two rectangles overlap
overlap :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> Bool
overlap (xMin1, xMax1, yMin1, yMax1) (xMin2, xMax2, yMin2, yMax2) =
  not (xMax1 < xMin2 || xMin1 > xMax2 || yMax1 < yMin2 || yMin1 > yMax2)

detectCollisions :: QuadTree -> Boid -> Int -> [Boid]
detectCollisions quadTree boid depth
  | depth > maxDepth = []  -- Stop further recursion if maxDepth is exceeded
  | otherwise = filter (\other -> boid /= other && distanceEU (pos boid) (pos other) < collisionRange) nearbyBoids
  where
    -- Define the search bounds
    V2 x y = pos boid
    collisionRange = protectedRange
    searchBounds = (x - collisionRange, x + collisionRange, y - collisionRange, y + collisionRange)

    -- Query the quad tree for nearby boids
    nearbyBoids = queryQuadTree quadTree searchBounds depth

--------------------------------------
-- Parameters
--------------------------------------

scalealignmen :: Float
scalealignmen = 0.2

visualRange:: Float
visualRange = 0.8

protectedRange :: Float
protectedRange = 0.15

avoidfactor :: Float
avoidfactor = 0.01 

centeringfactor :: Float
centeringfactor = 0.2

turnfactor :: Float
turnfactor = 0.037
