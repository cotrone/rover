module Navigation where

import Control.Monad
import Control.Monad.Loops
import Geo.Computations
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Data.Bool


data RoverState =
    TowardsPoint Point -- ^ We have a current point to travel to and are safely inside bounds
  | Lost Point         -- ^ We have traveled too far outside of the bounds
  | Relocating Heading Point -- ^ We are attempting to get back on the path

-- | Maximum angle that the rover will reset
-- the location on the line
maximumResetAngle :: Double
maximumResetAngle = 10.0

-- | meters the rover will continue
-- to reach the goal from
-- if it becomes lost or has to recalculate
-- it will count it as completed
-- if it is within the tolerance
tolerance :: Double
tolerance = 1.0

-- | Meters that the rover will consider the 
-- point completed if it is within
completedZone :: Double
completedZone = 1

data RoverInstruction =
    SetHeading Heading
  | Completed
  deriving(Eq,Ord,Show)

stepDrive :: (Point, Point) -> Point -> Heading -> RoverInstruction
stepDrive (start, end) current currentHeading =
  fromMaybe newHeading $ completed
  where
    completed =
      if abs (distance current end) < completedZone
        then Just Completed
        else Nothing
    headingToEnd = heading current end
    angle = angleBetween start end current
    newHeading = SetHeading $ headingToEnd - (5 * angle)

start, end :: Point
start = pt 35.2121686 (-97.4486838) Nothing Nothing
end = pt 35.211538 (-97.4419059) Nothing Nothing

showLatLong :: Point -> String
showLatLong (Point lat long _ _) = show lat ++ "," ++ show long

driveSimulate :: (Point, Point) -> IO Point
driveSimulate (st,en) = do
  snd <$> iterateUntilM ((==) Completed . fst) step (SetHeading initalHeading, st)
  where
    initalHeading = heading st en
    progressPoint h p = addVector (0.5, h) p 
    step i@(_, p) = do
      case i of
        (Completed,p) -> do
          -- putStrLn $ "I'm done at point: " ++ show p
          return (Completed, p)
        (SetHeading h, p) -> do
          putStrLn $ showLatLong p
          -- putStrLn $ "I've set my heading at: " ++ show (h,p)
          stepHeading h p
    stepHeading h p = do
      let
        p' = addVector (0.1,south) (progressPoint h p)
      return $ (stepDrive (st,en) p' h, p')

vplus :: Vector -> Vector -> Vector
vplus v1 v2 = fromPoints (x1 + x2, y1 + y2)
  where
    (x1, y1) = toPoints v1
    (x2, y2) = toPoints v2


fromPoints :: (Double, Double) -> Vector
fromPoints (x,y) = (l, h)
  where
    l = sqrt $! x ^ 2 + y ^ 2
    h = acos $! x / l

toPoints :: Vector -> (Double, Double)
toPoints (d, h) = (d * cos h, d * sin h)

angleBetween :: Point -> Point -> Point -> Heading
angleBetween p1 p2 p3 = heading p2 p1 - heading p2 p3

toDegrees :: Double -> Double
toDegrees r = (r / pi) * 180

fromDegrees :: Double -> Double
fromDegrees d = (d / 180) * pi
