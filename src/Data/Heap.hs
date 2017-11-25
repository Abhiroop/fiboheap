module Data.Heap
     ( FibHeap
     , empty
     , minimum
     , insert
     , union
     , extractMin
     ) where

import qualified Data.IntMap.Lazy as IMap
import Prelude hiding (minimum)

-- | A circular doubly linked list represented like Okasaki's lazy queues.

--               anti-clockwise
--                    |
--                    v
data Wheel a = Wheel [a] a [a] | EmptyWheel deriving Show
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
extractW (Wheel [] head [])      = (head, EmptyWheel)
extractW (Wheel acw head [])
  = (head , Wheel [] (last acw) (drop 1 (reverse acw)))
extractW (Wheel acw head (x:xs)) = (head, Wheel acw x xs)

-- | concatenate two wheels
-- Amortized O(1) because the reverse loads a thunk
concatW :: Wheel a -> Wheel a -> Wheel a
concatW w EmptyWheel = w
concatW EmptyWheel w = w
concatW (Wheel acw head cw) (Wheel acw' head' cw') =
  Wheel ((head' : cw') ++ (reverse acw')) head (cw ++ (reverse acw))

-- | The fibonacci heap is a recursive structure with a root wheel, its degree and associated subheaps

data FibHeap a = FibHeap (Wheel (Node a)) deriving Show

--         degree of the wheel
--                 |
--                 v

type Node a = (a, Int, FibHeap a)
--             ^          ^
--             |          |
--            key         |
--                 associated sub heap

-- | The empty fibonacci heap
empty :: FibHeap a
empty = FibHeap EmptyWheel

-- | The minimum element at the top of the heap
-- O(1)
minimum :: FibHeap a -> a
minimum (FibHeap EmptyWheel) = error "No elements in heap"
minimum (FibHeap (Wheel _ (x, _, _) _ )) = x

-- | Inset a new value onto the heap
-- Amortized O(1)
insert :: Ord a => a -> FibHeap a -> FibHeap a
insert x (FibHeap EmptyWheel) = FibHeap (Wheel [] (x, 0, empty)[])
insert x (FibHeap (Wheel acw n@(head, _, _) cw))
  | x < head  = FibHeap (Wheel acw (x, 0, empty) (n : cw))
  | otherwise = FibHeap $ goRight (Wheel acw (x,0,empty) (n : cw))

-- | Union of 2 heaps
-- Amortized O(1)
union :: Ord a => FibHeap a -> FibHeap a -> FibHeap a
union (FibHeap EmptyWheel) (FibHeap w) = FibHeap w
union (FibHeap w) (FibHeap EmptyWheel) = FibHeap w
union (FibHeap w1@(Wheel _ (h, _, _) _)) (FibHeap w2@(Wheel _ (h', _, _) _))
  | h < h'    = FibHeap $ concatW w1 w2
  | otherwise = FibHeap $ concatW w2 w1

-- The trick: When we are concatenating order of w' and w'' doesn't matter because consolidation takes care of marking the min
-- Consolidation also ensures that the root wheel doesn't contain nodes of the same degree.

-- | Extracting the minimum element of the heap
-- O(log n)
extractMin :: Ord a => FibHeap a -> (a, FibHeap a)
extractMin (FibHeap EmptyWheel) = error "Heap contains no elements"
extractMin (FibHeap (Wheel [] (x, _, h) [])) = (x,h)
extractMin (FibHeap w) = let ((x,_, (FibHeap w'')), w') = extractW w
                           in (x, consolidate $ FibHeap $ concatW w' w'')

-- | Convert root wheel to DA and translate DA to wheel finally
consolidate :: Ord a => FibHeap a -> FibHeap a
consolidate (FibHeap w) = FibHeap $ wheelDA $ makeDA w

-- | Convert  DA to wheel
wheelDA :: Ord a => IMap.IntMap (Node a) -> Wheel (Node a)
wheelDA m = foldr insNode EmptyWheel m

-- | ensure the head is the minimum
insNode :: Ord a => Node a -> Wheel (Node a) -> Wheel (Node a)
insNode n EmptyWheel = insertW n EmptyWheel
insNode n@(a,_,_) w@(Wheel _ (h,_,_) _)
  | a <= h = insertW n w
  | otherwise = goRight $ insertW n w

-- | Convert wheel to DA and make sure nodes of same degree are not on the root wheel
makeDA :: Ord a => Wheel (Node a) -> IMap.IntMap (Node a)
makeDA EmptyWheel = IMap.empty
makeDA w          = let (n, w') = extractW w
                     in insDA n (makeDA w')

-- | Ensuring that nodes of same degree gets linked
insDA :: Ord a => Node a -> IMap.IntMap (Node a) -> IMap.IntMap (Node a)
insDA n@(_,d,_) m = IMap.insertWith linkDA d n m

-- | Simple logic for linking 2 nodes
linkDA :: Ord a => Node a -> Node a -> Node a
linkDA n@(a,d,(FibHeap w)) n'@(a',d',(FibHeap w'))
  | a < a'    = (a , d+1, FibHeap $ insertW n' w)
  | otherwise = (a', d+1, FibHeap $ insertW n w')
