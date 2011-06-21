import Data.List
import qualified Data.Vector.Unboxed as U

type Point = (Double,Double)
type Line = (Double,Double)

pointsToLine :: Point -> Point -> (Double,Double)
pointsToLine (x,y) (x',y') = (slope,offset) where
                             slope = (y'-y)/(x'-x)
                             offset = if isInfinite slope then x else y-(x*slope)

orthoDist :: Point -> Line -> Double
orthoDist (x,y) (slope,offset) 
  | isInfinite slope = abs (x-offset)
  | slope == 0 = abs (y-offset)
  | otherwise = distance (newx,newy) (x,y) 
  where newslope = -(1.0)/slope
        newoffset = y-(newslope*x)
        newx = (offset-newoffset)/(newslope-slope)
        newy = (newx*newslope)+newoffset
        distance (a,b) (a',b') = sqrt(((a'-a)^2)+((b'-b)^2))

enpeuck :: Double -> [Point]-> [Point]
enpeuck _ [] = []
enpeuck thr pts
  | d < thr = [head pts,last pts]
  | otherwise = let (halfa,halfb) = break (==farPt) pts in (enpeuck thr (halfa++[farPt])) ++ (tail $ enpeuck thr halfb)
  where line = pointsToLine (head pts) (last pts)
        (farPt,d) = maximumBy (\(m,n) (m',n') -> compare n n') $ map (\x -> (x,orthoDist x line)) pts

enpeuckV :: Double -> U.Vector Point-> U.Vector Point
enpeuckV thr pts
  | pts == U.empty = U.empty
  | d < thr = U.cons (U.head pts) $ U.cons (U.last pts) U.empty
  | otherwise = let (halfa,halfb) = U.break (==farPt) pts in (enpeuckV thr (U.snoc halfa farPt)) U.++ (U.tail $ enpeuckV thr halfb)
  where line = pointsToLine (U.head pts) (U.last pts)
        (farPt,d) = U.maximumBy (\(m,n) (m',n') -> compare n n') $ U.map (\x -> (x,orthoDist x line)) pts
