{- |
Graffiti: a half-decent graph library with Ord vertices.

This is a directed graph and allows for self-loops. It's optimized for hasNode,
hasEdge, insertNode, insertEdge, and Neighbors, all of which are O(log(n)).

-}

module Graffiti(Graph
               ,empty
               ,fromVertexList
               ,hasNode
               ,hasEdge
               ,insertNode
               ,insertEdge
               ,uInsertEdge
               ,neighbors
               ,softInsertNode
               ,softInsertEdge
               ,softUInsertEdge
               ,softNeighbors
               ,toEdgeList
               ,fromEdgeList
               ,toVertexList
               ,transpose
               ,reachable
               ,test
               ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Monad.State as State
import qualified Control.Monad as Monad
import Data.Tuple(swap)
import Debug.Trace

type Graph v = Map.Map v (Set.Set v)

empty = Map.empty

isEmpty :: (Ord v) => Graph v -> Bool
isEmpty = Map.null

-- | creates a graph of only vertices (no edges)
fromVertexList :: (Ord v) => [v] -> Graph v
fromVertexList vertices = foldl (flip softInsertNode) empty vertices

hasNode :: (Ord v) => v -> Graph v -> Bool
hasNode v g = Map.member v g

hasEdge :: (Ord v) => v -> v -> Graph v -> Bool
hasEdge v w g =
  Set.member w (g Map.! v)

insertNode :: (Ord v) => v -> Graph v -> Graph v
insertNode v g =
  if hasNode v g
    then error "Attempt to insert node into already-present graph"
    else Map.insert v Set.empty g

-- | If v is already in the graph, do nothing, rather than throwing an error.
softInsertNode :: (Ord v) => v -> Graph v -> Graph v
softInsertNode v g =
  Map.insert v (softNeighbors v g) g

-- | Inserts an edge from v to w in g.
insertEdge :: (Ord v) => v -> v -> Graph v -> Graph v
insertEdge v w g =
  Map.insert v (Set.insert w (g Map.! v)) g

-- | If v or w are not it the graph, they will be added.
softInsertEdge :: (Ord v) => v -> v -> Graph v -> Graph v
softInsertEdge v w g =
  insertEdge v w $ softInsertNode w $ softInsertNode v g

-- | If v or w are not in graph, they will be added with indirected edge
softUInsertEdge :: (Ord v) => v -> v -> Graph v -> Graph v
softUInsertEdge v w g =
  insertEdge w v $ insertEdge v w $ softInsertNode w $ softInsertNode v g

-- | Inserts an undirected edge between v and w in g
uInsertEdge :: (Ord v) => v -> v -> Graph v -> Graph v
uInsertEdge v w g = insertEdge w v $ insertEdge v w g

neighbors :: (Ord v) => v -> Graph v -> Set.Set v
neighbors v g = g Map.! v

-- | If v is not in the graph, return Set.empty.
softNeighbors :: (Ord v) => v -> Graph v -> Set.Set v
softNeighbors v g =
  if hasNode v g
     then neighbors v g
     else Set.empty

-- | Returns a list of the vertices in the graph
vertices :: (Ord v) => Graph v -> [v]
vertices g = Map.keys g

toEdgeList :: (Ord v) => Graph v -> [(v, v)]
toEdgeList g =
  let keySetPairs = Map.toList g
      zipped = map (\(k, s) -> zip (repeat k) $ Set.toList s) keySetPairs
  in concat zipped

toVertexList :: (Ord v) => Graph v -> [v]
toVertexList g = Map.keys g

-- | Might not be the most efficient implementation, since it repeatedly calls
--   Set.union.
fromEdgeList :: (Ord v) => [(v, v)] -> Graph v
fromEdgeList l =
  let values = map snd l
      keySingletons = map (\(k, v) -> (k, Set.singleton v)) l
      -- Ensure that all values are in the map as well.
      valuesEmptysets = map (\v -> (v, Set.empty)) values
  in Map.fromListWith Set.union $ keySingletons ++ valuesEmptysets

-- | Returns a copy of the graph with all its edges reversed.
transpose :: (Ord v) => Graph v -> Graph v
transpose g =
  let egi = fromEdgeList $ map swap $ toEdgeList g
  in foldl (\g v -> softInsertNode v g) egi $ toVertexList g

-- | Returns all w such that exists v in s such that there is a path from v to
--   w in g.
reachable :: (Ord v) => Set.Set v -> Graph v -> Set.Set v
reachable s g =
  let nss = map (\v -> Set.toList $ neighbors v g) $ Set.toList s
      ns = concat nss
  in snd $ State.runState (mapM_ (exploreUnvisited g) ns) s

exploreUnvisited :: (Ord v) => Graph v -> v -> State.State (Set.Set v) ()
exploreUnvisited g v = do
  seen <- State.get
  if Set.member v seen
     then return ()
     else do
       State.put $ Set.insert v seen
       mapM_ (exploreUnvisited g) $ Set.toList $ neighbors v g

-- | Removes a vertex from the graph
uRemoveVertex :: (Ord v) => Graph v -> v -> Graph v
uRemoveVertex g v =
  let
    updatedG = Map.delete v g
    ns = Set.toList $ neighbors v g
  in foldl (\g n -> Map.insert n (Set.delete v (g Map.! n)) g) updatedG ns


assert :: String -> Bool -> Either String ()
assert s True = Right ()
assert s False = Left s

assertEq :: (Show a, Eq a) => s -> a -> a -> Either String ()
assertEq s x y =
  if x == y
  then Right ()
  else Left $ show x ++ " /= " ++ show y

test :: Either String ()
test = do
  let g = fromEdgeList [(0, 1), (1, 2), (2, 0), (2, 3), (4, 0)]
  assert "edges inserted" $ hasEdge 0 1 g
  assert "edges inserted" $ hasEdge 1 2 g
  assert "edges inserted" $ hasEdge 2 0 g
  assert "edges inserted" $ hasEdge 2 3 g
  assert "edges inserted" $ hasEdge 4 0 g
  assert "edges are directed" $ not $ hasEdge 2 1 g
  assert "edges not added randomly" $ not $ hasEdge 0 3 g
  assertEq "reachable1" (reachable (Set.fromList [0]) g)
                        (Set.fromList [0, 1, 2, 3])
  assertEq "reachable2" (reachable (Set.fromList [4]) g)
                        (Set.fromList [0, 1, 2, 3, 4])
  assertEq "reachable3" (reachable (Set.fromList [3]) g) (Set.fromList [3])
  assertEq "reachable4" (reachable (Set.fromList []) g) (Set.fromList [])
  assertEq "neighbors0" (neighbors 0 g) (Set.fromList [1])
  assertEq "neighbors1" (neighbors 1 g) (Set.fromList [2])
  assertEq "neighbors2" (neighbors 2 g) (Set.fromList [0, 3])
  assertEq "neighbors3" (neighbors 3 g) (Set.fromList [])
  assertEq "neighbors4" (neighbors 4 g) (Set.fromList [0])
  let g' = transpose g
  assert "transpose: edges inserted" $ hasEdge 1 0 g'
  assert "transpose: edges inserted" $ hasEdge 2 1 g'
  assert "transpose: edges inserted" $ hasEdge 3 2 g'
  assert "transpose: edges inserted" $ hasEdge 0 4 g'
  assert "transpose: edges are directed" $ not $ hasEdge 1 2 g'
  assert "transpose: edges not added randomly" $ not $ hasEdge 3 0 g'
