module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point
import Graphics.Gloss.Data.Vector

-- Constants

window = FullScreen :: Display
fps = 60 :: Int
background = black :: Color
g = 6.67e-11:: Float
positionScale = 6.15e-7 :: Float -- px/m
planetSizeScale = 50 :: Float
speed = 250 :: Int
        

type Position = Point.Point
type Velocity = Point.Point
type Mass = Float
data Body     = Body {
    position    :: Position,
    velocity    :: Velocity,
    mass        :: Mass
} deriving Show 
newtype Simulation = Simulation {
    bodies       :: [Body]
}


initialState :: Simulation
initialState = Simulation {
    bodies = [planet, moon, vehicle]
    }
    where
        planet = Body {
            position = (0, 0),
            velocity = (0, 0),
            mass = 5.97219e24
                }
        moon = Body {
            position = (0, 384403000),
            velocity = (-1018, 0),
            mass = 7.347673e22
                }
        vehicle = Body {
            position = (-1e8, -3e8),
            velocity = (1000, 550),
            mass = 10000
                }

render :: Simulation -> Picture
render simulation = pictures $ objects ++ [translate (-700) 400 $ scale 0.18 0.18 $ color white $ text "ESC to exit"]
    where
        makeObject :: Color -> Body -> Picture
        makeObject col Body {position=(x, y), mass=mass} =
            translate (scalePosition x) (scalePosition y) $ color col $ circleSolid $ scaleMass mass
            where 
                scaleMass :: Float -> Float
                scaleMass = logBase planetSizeScale
                scalePosition :: Float -> Float
                scalePosition val = positionScale * val
                
        colors = [blue, white, red]
        objects = zipWith makeObject colors $ bodies simulation
    
-- -- -- --     
    
countDistance :: Position -> Position -> Float
countDistance position1 position2 = magV $ position1 Point.- position2

countForce :: Body -> Body -> Vector
countForce body1 body2 = 
        if r < 1 then (0.0, 0.0) 
                 else ((g * mass body1 * mass body2) / (r^2)) Point.* unit
    where
        r = countDistance (position body2) (position body1)
        unit = normalizeV $ position body2 Point.- position body1
 
countAcceleration :: Body -> Body -> Point.Point
countAcceleration b1 b2 =  1/mass b1 Point.* countForce b1 b2
  
countAccelerationSum :: [Body] -> Body -> Point.Point 
countAccelerationSum bodies body = a
    where
        a = foldr ((Point.+) . countAcceleration body) (0.0, 0.0) bodies       
  
countVelocity :: Float -> (Body, Point.Point) -> Body
countVelocity seconds (Body{velocity = v1, position = p1, mass = m1}, acceleration) = 
    Body{ 
        velocity = v1 Point.+ (seconds Point.* acceleration),
        position = p1,
        mass = m1
    }

countPosition :: Float -> Body -> Body
countPosition seconds Body{velocity = v1, position = p1, mass = m1} = 
    Body{
        position = p1 Point.+ (seconds Point.* v1),
        velocity = v1,
        mass = m1
    }
             
move :: Float -> Simulation -> Simulation
move seconds Simulation {bodies=bodies} = Simulation {
    bodies      = bodies2
    }        
    where
        seconds1 = fromIntegral speed * seconds
        accelerations = map (countAccelerationSum bodies) bodies
        bodies1 = map (countVelocity seconds1) (zip bodies accelerations)
        bodies2 = map (countPosition seconds1) bodies1

              
main :: IO ()
main = simulate window background fps initialState render update
    where
        update :: ViewPort -> Float -> Simulation -> Simulation
        update _ time = fpow speed $ move time
            where
                fpow n f x = iterate f x !! n    
