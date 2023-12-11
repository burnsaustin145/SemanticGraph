# Semantic Graph in Haskell
This program performs the following operations
- `>add node_name` -> adds a node to the graph
- `>relation domain_node relation range_node` -> creates a labeled edge
- `>query range_node` -> returns all domain nodes with range_node in the range
- `>infer node1 node2` -> creates a hypernode with label 'node1.node2', where the edges are the intersection of node1 edges and node2 edges
