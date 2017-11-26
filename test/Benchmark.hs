module Benchmark where

import Data.Heap

listHeap :: Ord a => [a] -> FibHeap a
listHeap l = foldr (insert) empty l

heapList :: Ord a => FibHeap a -> [a]
heapList fh = let x = extractMin fh
                in case x of
                     Nothing -> []
                     Just (a,b) -> a : heapList b

heapSort :: Ord a => [a] -> [a]
heapSort = heapList . listHeap 

