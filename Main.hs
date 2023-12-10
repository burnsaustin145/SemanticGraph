import System.IO (isEOF)
import Graph

main :: IO ()
main = loop (Graph [])

loop :: Graph -> IO ()
loop graph = do
    putStrLn "Enter (add, relation, infer, reveal, query, q):"
    command <- getLine
    case words command of
        ("add":ns) -> if null ns  
                      then putStrLn "Invalid command" >> loop graph
                      else loop (addNode (head ns) graph)
        ("relation":ns) -> if null ns || length ns /= 3
                      then putStrLn "Invalid command" >> loop graph
                      else loop (addEdge (ns !! 0) (ns !! 2) (ns !! 1) graph)
        ("infer":ns) -> if null ns || length ns /= 2
                      then putStrLn "Invalid command" >> loop graph
                      else loop ((infer (ns !! 0) (ns !! 1)) graph)
        ("reveal":_)     -> (printGraph graph) >> loop graph
        ("query":ns)  -> if null ns || length ns /= 1
                      then putStrLn "Invalid command" >> loop graph
                      else (queryGraph (ns !! 0) graph) >> loop graph
        ("q":_)     -> return ()
        _              -> putStrLn "Invalid command" >> loop graph
