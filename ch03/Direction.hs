data Point = Point Int Int

data Direction = Direction Point Point
				 deriving(Show)

direction (Direction a b) = directionByPoint a b
							where
								directionByPoint (Point x1 y1) (Point x2 y2) = 
							