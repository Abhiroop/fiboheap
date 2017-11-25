module Benchmark where

import Data.Heap

listHeap :: Ord a => [a] -> FibHeap a
listHeap l = foldr (insert) empty l

heapList :: Ord a => FibHeap a -> [a]
heapList fh = let (a,b) = extractMin fh
                in (a : heapList b)

heapSort :: Ord a => [a] -> [a]
heapSort = heapList . listHeap 

