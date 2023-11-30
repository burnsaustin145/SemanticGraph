module Graph where

    -- Graph: Adjacency list representation, labeled directed graph
    --
    --		 [node1 [(range11, relation11), (range12, relation12), ... ,(range_1i, relation_1i),
    --        node2 [(range22, relation22), (range22, relation22), ... ,(range_2k, relation_2k)
    --		  ...
    --        nodeN [(rangeN1, relationN1), (rangeN2, relationN2), ... ,(range_Nj, relationNj)]
    --
    -- Hypernode: node label is delimited by ','
    --       ['label1,label2,...,labelL' [(shared_range1, shared_relation1), ... ,(shared_rangeQ, shared_relQ)]]

    -- Edge (rangenode, edgelabel)
    data Edge = Edge (String, String) deriving (Eq, Show)
    -- Node "Name" [list of edges]
    data Node = Node String [Edge] deriving (Show)
    -- Graph List of nodes
    data Graph = Graph [Node] deriving (Show)

    -- Graph functions

    -- add node by string label
    addNode :: String -> Graph -> Graph
    addNode str graph = appendNode (parseNode str) graph
    -- helper to append a node type to the main graph list
    appendNode :: Node -> Graph -> Graph
    appendNode n (Graph ns) = Graph (ns ++ [n])

    -- parse string to node with identity edgelist
    parseNode :: String -> Node
    parseNode str = Node str (idEdgeList str)
    -- Identity edgelist adds node from string along with the identity edge
    idEdgeList :: String -> [Edge]
    idEdgeList str = [Edge (str, "id")]

    
    -- gets string title from node
    getNodeLabel :: Node -> String
    getNodeLabel (Node str _ ) = str

    -- Function to add an edge to the graph
    addEdge :: String -> String -> String -> Graph -> Graph
    addEdge curr toNode arcVal (Graph [g]) 
                | curr == getNodeLabel g = Graph [addEdgeHelper toNode arcVal g]
                | curr /= getNodeLabel g = addEdge curr toNode arcVal (addNode toNode (Graph [g]))
    addEdge curr toNode arcVal (Graph ns)
                | curr == getNodeLabel (head ns) = Graph ((addEdgeHelper toNode arcVal (head ns)) :  (tail ns))
                | curr /= getNodeLabel (head ns) = appendNode (head ns) (addEdge curr toNode arcVal (Graph (tail ns)))
    -- append the new edge to the node edgelist once found
    addEdgeHelper :: String -> String -> Node -> Node
    addEdgeHelper str arcVal (Node label (x:xs)) = Node label ((Edge (str, arcVal)) : (x:xs))

    -- takes two nodes and makes a hypernode of shared edges
    infer :: String -> String -> Graph -> Graph
    infer nodeA nodeB ns = inferHelper (nodeA ++ "," ++ nodeB) (sharedEdges (findNode nodeA ns) (findNode nodeB ns)) 
                                         (addNode (nodeA ++ "," ++ nodeB) ns) 
    -- takes the new hypernode and the shared edgelist and appends edges to the graph
    inferHelper :: String -> Maybe [Edge] -> Graph -> Graph
    inferHelper node Nothing ns = ns
    inferHelper node (Just []) ns = ns
    inferHelper node (Just [Edge (range, arcVal)]) ns = (addEdge node range arcVal ns)
    inferHelper node (Just ((Edge (range, arcVal)):es)) ns = inferHelper node (Just es) (addEdge node range arcVal ns)

    -- takes a string as node label and returns the node from the graph
    findNode :: String -> Graph -> Maybe Node
    findNode label (Graph []) = Nothing
    findNode label (Graph [n]) 
                | label == getNodeLabel n = Just n
                | label /= getNodeLabel n = Nothing
    findNode label (Graph ns)
                | label == getNodeLabel (head ns) = Just (head ns)
                | otherwise = (findNode label (Graph (tail ns))) 

    -- find shared edges
    sharedEdges :: Maybe Node -> Maybe Node -> Maybe [Edge]
    sharedEdges Nothing Nothing = Nothing
    sharedEdges (Just (Node n1 (x:xs))) (Just (Node n2 (n:ns))) = Just (sharedEdgesHelper (x:xs) (n:ns))

    -- takes two edge lists and returns the intersection
    sharedEdgesHelper :: [Edge] -> [Edge] -> [Edge]
    sharedEdgesHelper xs ns = [x | x <- xs, elem x ns]

    -- returns boolean value for edge equality
    edgeEquality :: Edge -> Edge -> Bool
    edgeEquality (Edge (range1, relation1)) (Edge (range2, relation2)) = if 
    	(range1 == range2) && (relation1 == relation2)
    	    then True
    	    else False

    -- function for Querying graph
    -- input string is range node name
    queryGraph :: String -> Graph -> IO()
    queryGraph rangeNode graph = printGraph (queryGraphHelper rangeNode graph)
    -- takes rangeNode label and gets a subgraph of domain nodes
    queryGraphHelper :: String -> Graph -> Graph
    queryGraphHelper rangeNode (Graph []) = Graph []
    queryGraphHelper rangeNode (Graph ns)
                | isInEdges rangeNode (head ns) = Graph ((head ns) : nodeList)
                | otherwise = Graph nodeList
                where
                    Graph nodeList = queryGraphHelper rangeNode (Graph (tail ns))
    -- returns true if rangeNode is in edges
    isInEdges :: String -> Node -> Bool
    isInEdges rangeNode (Node _ edges) = isInEdgesHelper rangeNode edges

    isInEdgesHelper :: String -> [Edge] -> Bool
    isInEdgesHelper _ [] = False
    isInEdgesHelper rangeNode (Edge (range, _):es)
                | rangeNode == range = True
                | otherwise = isInEdgesHelper rangeNode es

    -- Printing Functions
    printGraph :: Graph -> IO()
    printGraph (Graph nodes) = mapM_ printNode nodes

    printNode :: Node -> IO ()
    printNode (Node name edges) = do
    	putStrLn ("\n" ++ name)
    	mapM_ (putStrLn . formatEdge) edges

    formatEdge :: Edge -> String
    formatEdge (Edge (rangeNode, relation)) = "        " ++ relation ++ "   ---> " ++ rangeNode ++ "\n"
    

