import System.Random
import qualified Data.List as L

---- PART 1 ----

-- Define type
type Point = (Float, Float)

-- Velocity at a given supporting point
velocity :: Float -> Float -> Float
velocity y0 yi = sqrt(9.81*(y0 - yi)*2)

-- Euclidian distance calculation between two user given points
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

-- Takes distance and 2 velocities and returns sliding time
time :: Float -> Float -> Float -> Float
time distance vi vj = distance / (vi+vj/2)

-- Takes start and end points and list of supporting points which returns a list of pairs
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs start end points = [(start, head points)] ++ pairs points ++ [((last points), (end))]

-- Calculates the total time by summing the points between each supporting point
total_time :: Point -> Point -> [Point] -> Float
total_time (x, y) (xi, yi) points = sum (map time_at_point (arrange_in_pairs (x, y) (xi, yi) points))

time_at_point :: (Point, Point) -> Float
time_at_point ((x, y), (xi, yi)) = time d vi vj
  where d = distance (x, y) (xi, yi)
        vi = velocity x y
        vj = velocity xi yi


----  PART 2 ----

-- Define type candidate, 4-tuple of two points, list of points and a Float
type Candidate = (Point, Point, [Point], Float)

{-- Takes start, end and supporting points and
creates a candidate with slide time. Creates list with same amount of pairs --}
make_candidates :: Point -> Point -> [[Point]] -> Candidate
make_candidates start end [points] = (start, end, points, total_time start end points)

-- Sort list of sliding times into ascending order (imported Data.List to ease the function)
sort_by_time :: [Candidate] -> [Candidate]
sort_by_time list = L.sort list

{-- Creates a string representation of the candidate. Each point is written as a
new line. Time is indicated in an extra line following --}
first_elem :: Candidate -> Point -- To get the start point
first_elem (x,_,_,_) = x

second_elem :: Candidate -> Point -- To get the end point
second_elem (_,x,_,_) = x

third_elem :: Candidate -> [Point] -- List variation signature of above to get supporting points
third_elem (_,_,x,_) = x

last_elem :: Candidate -> Float
last_elem (_,_,_,x) = x

float_to_string :: Float -> String
float_to_string float = show float

point_to_string :: Point -> String   -- Takes a Point and converts to string
point_to_string point = show (fst point) ++ " " ++ show (snd point)

list_to_string :: [Point] -> String   -- Converts a list of 2-tuples to string
list_to_string (x:xs) = point_to_string x ++ "\n" ++ list_to_string xs

candidate_to_string :: Candidate -> String
candidate_to_string list = point_to_string (first_elem list) ++ "\n" ++ list_to_string (third_elem list) ++ "\n" ++ point_to_string (second_elem list) ++ "\n" ++ "Time: " ++ float_to_string (last_elem list)

{-- Takes two lists and creates a list if list of points. Each uses first list
as x and the next n of 2nd list as y, where n is length of first list --}
divide_list :: [Float] -> [Float] -> [[Point]]
divide_list _ [] = []
divide_list xs ys = [ [(x, y)] | x <- xs, y <- ys]

---- PART 3 ----

{-- Part 3 does not run as intended and may cause file to not compile, in which
case. Delete it's contents. Or run Part 1 and 2 independantly from Part 3--}
-- Creates a list of random floats
random_list :: Float −> (Float, Float) −> StdGen −> ([Float], StdGen)
random_list 0 _ gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
  where
      ( r , g) = randomR minmax gen
      ( rs, g2) = random_list (n−1) minmax g

{-- Randomly populates candidates. The input is number of functions needed, start
and end points of slide. As well as min and max for numbers to gen. --}
create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float, Float) -> StdGen -> ([Candidate], StdGen)
create_random_candidates n start end xvalues (minval, maxval) rn = (make_candidates start end random_list) minval maxval StdGen

