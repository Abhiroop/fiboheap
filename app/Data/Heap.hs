module Data.Heap where

-- | A circular doubly linked list represented like Okasaki's lazy queues.

--               anti-clockwise
--                    |
--                    v
data Wheel a = Wheel [a] a [a] | EmptyWheel
--                          ^
--                          |
--                       clockwise


-- | Returns the head of the wheel.
-- O(1)
readW :: Wheel a -> a
readW EmptyWheel       = error "No elements in heap"
readW (Wheel _ head _) = head

-- | Move the head one place counter clockwise.
-- Amortized O(1)
goLeft :: Wheel a -> Wheel a
goLeft EmptyWheel         = error "No elements in heap"
goLeft (Wheel [] head []) = error "Heap too small"
goLeft (Wheel [] head cw)
  = Wheel (drop 1 (reverse cw)) (last cw) [head]
goLeft (Wheel (x:xs) head cw) = Wheel xs x (head : cw)

-- | Move the head one place clockwise.
-- Amortized O(1)
goRight :: Wheel a -> Wheel a
goRight EmptyWheel         = error "No elements in heap"
goRight (Wheel [] head []) = error "Heap too small"
goRight (Wheel acw head [])
  = Wheel [head] (last acw) (drop 1 (reverse acw))
goRight (Wheel acw head (x:xs)) = Wheel (head : acw) x xs

-- | insert a new element into a wheel, it will be the new head and shift right
-- O(1)
insertW :: a -> Wheel a -> Wheel a
insertW x EmptyWheel          = Wheel [] x []
insertW x (Wheel acw head cw) = Wheel acw x (head : cw)

-- | deletes the head of the wheel and returns it and makes the right most element the head
-- Amortized O(1)
extractW :: Wheel a -> (a, Wheel a)
extractW EmptyWheel              = error "No elements in heap" 
extractW (Wheel acw head [])
  = (head , Wheel [] (last acw) (drop 1 (reverse acw)))
extractW (Wheel acw head (x:xs)) = (head, Wheel acw x xs)

-- | concatenate two wheels
-- O(n)
concatW :: Wheel a -> Wheel a -> Wheel a
concatW w EmptyWheel = w
concatW EmptyWheel w = w
concatW (Wheel acw head cw) (Wheel acw' head' cw') =
  Wheel ((head' : cw') ++ (reverse acw')) head (cw ++ (reverse acw))

-- | The fibonacci heap is a recursive structure with a root wheel, its degree and associated subheaps
--                           degree of the wheel
--                                   |
--                                   v
data FibHeap a = FibHeap (Wheel (a, Int, FibHeap a))
--                               ^          ^
--                               |          |
--                              key         |
--                                   associated sub heap

-- | The empty fibonacci heap
empty :: FibHeap a
empty = FibHeap EmptyWheel

-- | The minimum element at the top of the heap
minimum :: FibHeap a -> a
minimum (FibHeap EmptyWheel) = error "No elements in heap"
minimum (FibHeap (Wheel _ (x, _, _) _ )) = x

-- | Inset a new value onto the heap
insert :: a -> FibHeap a -> FibHeap a
insert = undefined

-- | Union of 2 heaps
union :: FibHeap a -> FibHeap a -> FibHeap a
union = undefined

-- | Extracting the minimum element of the heap
extractMin :: FibHeap a -> (a, FibHeap a)
extractMin = undefined
