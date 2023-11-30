import System.IO (isEOF)
import Graph

main :: IO ()
main = loop (Graph [])

loop :: Graph -> IO ()
loop graph = do
    putStrLn "Enter (add, relation, infer, reveal, query, q):"
    command <- getLine
    case words command of
        ("add":ns) -> loop (addNode (read (head ns)) graph)
        ("relation":ns) -> loop (addEdge (read (ns !! 0)) (read (ns !! 2)) (read (ns !! 1)) graph)
        ("infer":ns) -> loop ((infer (read (ns !! 0)) (read (ns !! 1))) graph)
        ("reveal":_)     -> (printGraph graph) >> loop graph
        ("query":ns)  -> (queryGraph (read (ns !! 0)) graph) >> loop graph
        ("q":_)     -> return ()
        _              -> putStrLn "Invalid command" >> loop graph
