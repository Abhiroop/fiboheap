module Fibo where

-- | A circular doubly linked list.

--               anti-clockwise
--                    |
--                    v
data Wheel a = Wheel [a] [a]
--                        ^
--                        |
--                     clockwise


-- | Returns the head of the wheel.

readW :: Wheel a -> a
readW = undefined

-- | Move the head one place counter clockwise.

goLeft :: Wheel a -> Wheel a
goLeft = undefined

-- | Move the head one place clockwise.

goRight :: Wheel a -> Wheel a
goRight = undefined

-- | insert a new element into a wheel, it will be the new head

insertW :: a -> Wheel a -> Wheel a
insertW = undefined

-- | deletes the head of the wheel and returns it

extractW :: Wheel a -> (a, Wheel a)
extractW = undefined

-- | concatenate two wheels

concatW :: Wheel a -> Wheel a -> Wheel a
concatW = undefined
