-- file: ch03/BadTree.hs
data Node a = Node a (Node a) (Node a)
			  | Empty
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _ _ = Nothing

nodesAreSame (Node a _ _) (Node b _ _)
			 | a == b = Just a
nodesAreSame _ _ = Nothing