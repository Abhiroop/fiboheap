module Fibo where

-- | A circular doubly linked list.

--               anti-clockwise
--                    |
--                    v
data Wheel a = Wheel [a] a [a]
--                          ^
--                          |
--                       clockwise


-- | Returns the head of the wheel.
-- O(1)
readW :: Wheel a -> a
readW (Wheel _ head _) = head

-- | Move the head one place counter clockwise.
-- O(1)
goLeft :: Wheel a -> Wheel a
goLeft (Wheel (x:xs) head cw) = Wheel xs x (head : cw)

-- | Move the head one place clockwise.
-- O(1)
goRight :: Wheel a -> Wheel a
goRight (Wheel acw head (x:xs)) = Wheel (head : acw) x xs

-- | insert a new element into a wheel, it will be the new head and shift right
-- O(1)
insertW :: a -> Wheel a -> Wheel a
insertW x (Wheel acw head cw) = Wheel acw x (head : cw)

-- | deletes the head of the wheel and returns it
-- O(1)
extractW :: Wheel a -> (a, Wheel a)
extractW (Wheel acw head (x:xs)) = (x, Wheel acw head xs)

-- | concatenate two wheels
-- O(n)
concatW :: Wheel a -> Wheel a -> Wheel a
concatW (Wheel acw head cw) (Wheel acw' head' cw') =
  Wheel ((head' : cw') ++ acw) head (cw ++ acw)
