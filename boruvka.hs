module Main where

import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Bool
import Data.Maybe
import Data.Function
import Control.Parallel 
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Time
import GHC.Conc (numCapabilities)

type Node = Int
data Edge = Edge (Node, Node) deriving (Show, Eq)
data Wedge = Wedge (Edge, Float) deriving (Show, Eq)
data Wgraph = Wgraph [Wedge] deriving (Show)

instance NFData Wedge where
    rnf w = w `seq` ()

cores = fromIntegral numCapabilities

------------------------------------------------------------------------
----------------------------BUILD GRAPH---------------------------------
------------------------------------------------------------------------

-- Build a weighted edge
buildWedge :: [String] -> Wedge
buildWedge [e, n1, n2, d] = Wedge (Edge (read n1 :: Node, read n2 :: Node), read d :: Float)
buildWedge [e, n1, n2] = Wedge (Edge (read n1 :: Node, read n2 :: Node), 1)

-- From a DIMACS format text file, keeps only the edge while removing the rest
keepEdges :: [String] -> [String]
keepEdges str = filter (\e -> head e == 'e') str

-- Build a list of weighted edges from a list of String
fromLinestoWedge :: [String] -> [Wedge]
fromLinestoWedge str = map buildWedge (map words str)

-- Build a weighted graph from a list of weighted edges
buildGraph :: [Wedge] -> Wgraph
buildGraph wedges = Wgraph (wedges)

------------------------------------------------------------------------
-----------------------RETURNS EDGES/NODES/WNODES-----------------------
------------------------------------------------------------------------

-- Gives all the weighted edges within a graph
getEdges :: Wgraph -> [Wedge]
getEdges (Wgraph graph) = graph

-- Gives the two nodes linked by a weighted edge
twoNodes :: Wedge -> [Node]
twoNodes (Wedge (Edge (n1, n2), f)) = [n1, n2]

-- Gives the same thing than twoNodes but with several weighted edges
nodesForEdges :: [Wedge] -> [Node]
nodesForEdges wedges = nub (concat (map twoNodes wedges))

-- Gives all the nodes within a graph
getNodes :: Wgraph -> [Node]
getNodes graph = quickSort (nodesForEdges (getEdges graph))

-- From Node to [Node]
listOfNode :: Node -> [Node]
listOfNode node = [node]

-- Gives all the components within a graph
getComponents :: Wgraph -> [[Node]]
getComponents graph = map listOfNode (getNodes graph)

-- Tries to find an edge between two given nodes, returns it if it exists
tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph g) n1 n2  = find (\x -> [n1, n2] \\ twoNodes x == []) g

------------------------------------------------------------------------
-----------------------MATRIX REPRESENTATION/STRINGS--------------------
------------------------------------------------------------------------

-- Returns the matrix representation within strings
buildMatrixNotation :: Wgraph -> [String]
buildMatrixNotation graph = map (makeLineWedge graph (getNodes graph)) (getNodes graph)

-- Creates a String listing all the linked edges to a given node
makeLineWedge :: Wgraph -> [Node] -> Node -> String
makeLineWedge graph nodes n = intercalate "" (map (getWeightOrZero graph n) nodes)

-- Gets the weight of an wedge if it exists, 0 otherwise
getWeightOrZero :: Wgraph -> Node -> Node -> String
getWeightOrZero graph n n2 | wedge == Nothing = "0 "
                           | otherwise = show (round (weight (fromJust wedge))) ++ " "
                           where
                             wedge = tryGetWedge graph n n2

fromEdgeToString :: Edge -> String
fromEdgeToString (Edge (n1, n2)) = "Edge ("++show n1++","++show n2++")"

fromWedgeToString :: Wedge -> String
fromWedgeToString (Wedge (x1, x2)) = "Wedge ("++fromEdgeToString x1++","++show x2++"),"

------------------------------------------------------------------------
------------------------SELF CONNECTED NODES----------------------------
------------------------------------------------------------------------

-- Returns True if the given edge is self connected
loopWedge :: Wedge -> Bool
loopWedge (Wedge (Edge (n1, n2), f)) | n1 == n2  = True
                                     | otherwise = False

-- Returns False if a given graph has no self-connected edges, false otherwise
noLoopGraph :: Wgraph -> Bool
noLoopGraph graph | find (==True) (map loopWedge (getEdges graph)) == Just True = True
                  | otherwise                                                   = False

------------------------------------------------------------------------
---------------------------------LINKED---------------------------------
------------------------------------------------------------------------

-- Returns the weight of a weighted edge
weight :: Wedge -> Float
weight (Wedge (_, w)) = w

-- Gives the less weighted edge
minWeight :: [Wedge] -> Wedge
minWeight edges = minimumBy (compare `on` weight) edges

-- Returns a list of weighted edges linked to the given node in a graph 
linkedWedges :: Wgraph -> Node -> [Wedge]
linkedWedges graph n = filter (\e -> n `elem` twoNodes e) (getEdges graph)

-- Returns a list of weighted edges linked to a given component within a graph
linkedComponent :: Wgraph -> [Node] -> [Wedge]
linkedComponent g nodes = filter (\w -> not (((head (twoNodes w)) `elem` nodes) && (((twoNodes w)!!1) `elem` nodes))) (concat (map (linkedWedges g) nodes `using` parListChunk (length nodes `div` cores) rdeepseq))

-- Returns the less weighted edge linked to a given node
minLinkedWedges :: Wgraph -> Node -> Wedge
minLinkedWedges graph n = minWeight (linkedWedges graph n)

-- Returns the less weighted edge linked to a given component
minLinkedComponent :: Wgraph -> [Node] -> Wedge
minLinkedComponent graph component = minWeight (linkedComponent graph component)

-- Returns the less weighted edges linked to several components
minLinkedComponents :: Wgraph -> [[Node]] -> [Wedge]
minLinkedComponents graph components = map (\c -> minWeight (linkedComponent graph c)) components

-- Returns a list of components from a given list of edges
fromWedgesToNodes :: [Wedge] -> [[Node]]
fromWedgesToNodes wedges = map (\w -> twoNodes w) wedges

------------------------------------------------------------------------
---------------------------BORUVKA ALGORITHM----------------------------
------------------------------------------------------------------------

-- Launch the recursive boruvka algorithm
boruvka :: Wgraph -> [Wedge]
boruvka g = 
  let wedges = []
      components = getComponents g
      real_comp = []
      (_, _, _, wedges') = boruvkaAlg g components real_comp wedges
      in wedges'

-- Recursive boruvka algorithm which ends when the list of component is only composed of one component (all the nodes)
boruvkaAlg :: Wgraph -> [[Node]] -> [[Node]] -> [Wedge] -> (Wgraph, [[Node]], [[Node]], [Wedge])
boruvkaAlg g components real_comp wedges | real_comp == [getNodes g] = (g, [], [], wedges)
                                         | otherwise =
      let 
          currwedges' | wedges == [] && length components < cores = nub $ parMap rdeepseq (minLinkedComponent g) components
                      | wedges == [] && length components >= cores = nub (map (minLinkedComponent g) components `using` parListChunk (length components `div` cores) rdeepseq)
                      -- | wedges == [] && length components >= 32 = nub $ concat $ parMap rdeepseq (minLinkedComponents g) (chunk 8 components)
                      | wedges /= [] && length real_comp < cores = nub $ parMap rdeepseq (minLinkedComponent g) real_comp
                      | wedges /= [] && length real_comp >= cores = nub (map (minLinkedComponent g) real_comp `using` parListChunk (length real_comp `div` cores) rdeepseq)
                      -- | wedges /= [] && length real_comp >= 32 = nub $ concat $ parMap rdeepseq (minLinkedComponents g) (chunk 8 real_comp)
          --currwedges' = nub (map (minLinkedComponent g) components)
          wedges' | length real_comp == 2 = wedges ++ [head currwedges']
                  | otherwise = wedges ++ currwedges'
          midcomponents = fromWedgesToNodes currwedges'
          midcomponents' = rearrangeComponentFinal midcomponents
          real_comp' = rearrangeComponentFinal (real_comp ++ midcomponents')
      in boruvkaAlg g [] real_comp' wedges'

------------------------------------------------------------------------
---------------------------------LENGTH---------------------------------
------------------------------------------------------------------------

-- Returns the length of a graph
getDistance :: [Wedge] -> Float
getDistance wedges = sum (map weight wedges)

------------------------------------------------------------------------
----------------------------------SORT----------------------------------
------------------------------------------------------------------------

-- Basic quicksort function
quickSort :: (Ord a) => [a] -> [a]  
quickSort [] = []  
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]  
        biggerSorted = quickSort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

------------------------------------------------------------------------
----------------------------------LIST----------------------------------
------------------------------------------------------------------------

-- Chunk a list using the first parameter
chunk :: Int -> [[Node]] -> [[[Node]]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
        where 
                (y1, y2) = splitAt n xs

-- Concatenate two lists such that the elements in the resulting list occur only once
add :: [Node] -> [Node] -> [Node]
add l1 l2 = quickSort $ nub (l1 ++ l2)

-- Compare two given lists and returns true if some elements of the first are in the second
compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a

-- Modifies a list if two lists share elements
modifyIfDuplicate :: [Node] -> [Node] -> [Node]
modifyIfDuplicate l1 l2 | compareList l1 l2 = add l1 l2
                        | otherwise = l1

-- Checks for duplicates in the list of lists for a given list 
checkComponentDuplicate :: [[Node]] -> [Node] -> [Node]
checkComponentDuplicate mat nodes = 
   let new_mat = delete nodes mat
       in quickSort (nub (concat (map (modifyIfDuplicate nodes) new_mat)))
       
-- Rearrange the components modified by the boruvka algorithm at each step
rearrangeComponent :: [[Node]] -> [[Node]]
rearrangeComponent listoflist | length listoflist == 1 = listoflist
                              | length listoflist < 32 = nub (map (checkComponentDuplicate listoflist) listoflist `using` parListChunk (length listoflist `div` cores) rdeepseq)                             
                              | otherwise = nub (concat (map rearrangeComponent (chunk 8 listoflist) `using` parListChunk (length listoflist `div` cores) rdeepseq))

rearrangeComponentFinal :: [[Node]] -> [[Node]]
rearrangeComponentFinal listoflist | length listoflist == 1 = listoflist
                                   | otherwise = until (not . isThereStillDuplicates) (rearrangeComponent) listoflist

-- Checks whether a list of nodes has duplicates into a list of list
isThereDuplicatesNode :: [[Node]] -> [Node] -> Bool
isThereDuplicatesNode mat node | find (==True) (map (compareList node) new_mat) == Just True = True
                               | otherwise = False
                               where new_mat = delete node mat

-- Checks whether the list of lists of nodes still has duplicates
isThereStillDuplicates :: [[Node]] -> Bool
isThereStillDuplicates listoflist | length listoflist == 1 = False
                                  | find (==True) (map (isThereDuplicatesNode listoflist) listoflist) == Just True = True
                                  | otherwise = False


------------------------------------------------------------------------
----------------------------------MAIN----------------------------------
------------------------------------------------------------------------

main = do
            t0 <- getCurrentTime
            args <- getArgs
            content <- readFile (args !! 0)
            let file = (args !! 0)
            let file2 = take (length file - 4) file
            let linesOfFiles = lines content
            let graph = buildGraph (fromLinestoWedge (keepEdges linesOfFiles))
            
            --writeFile "edges.dat" (concat $ map fromWedgeToString (getEdges graph))
			
			--writeFile (file2 ++ ".mat") (unlines (buildMatrixNotation graph))
		
            let wedges = boruvka graph
            print (getNodes graph)
			
            t1 <- getCurrentTime
            --print (wedges)
            print (getDistance wedges)	
			
            t2 <- getCurrentTime
            
            print ("I/O Time : " ++ show (diffUTCTime t1 t0))
            print ("Boruvka Time : " ++ show (diffUTCTime t2 t1))
	    
            print ("Final")
            print (cores)
		
            {-let comp = map (minLinkedComponent graph) (getComponents graph)
            let w = fromWedgesToNodes comp
            let mid = rearrangeComponentFinal w
            let comp' = map (minLinkedComponent graph) mid
            let w' = fromWedgesToNodes comp'
            let mid' = rearrangeComponentFinal w'
            print (mid)-}
			
            --let mst_graph = buildGraph wedges
            
            --let mst = "mst"
            --writeFile (mst ++ ".mat") (unlines (buildMatrixNotation mst_graph))
