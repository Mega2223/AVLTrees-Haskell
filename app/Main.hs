f :: Int -> Int
f x = if x == 0 || x == 1 then x else f(x-1) + f(x-2)  

data Record = Record {value :: Int, name :: [Char]}
data Node = Node {label :: [Char], record :: Record}
data Arrow = Arrow {begin :: Node, end :: Node}
data Digraph = Digraph {nodes :: [Node], arrows :: [Arrow]}

showNode :: Node -> String
showNode = label

showArrow :: Arrow -> String
showArrow arrow = showNode (begin arrow) ++ "," ++ showNode (end arrow)

showDg :: Digraph -> String
showDg digraph = do
	let compNodes nds = do 
		if null nds then
			"?"
		else
			showNode(head nds) ++ "," ++ compNodes(tail nds)

	let compArrows ars = do
		if null ars then 
			"|"
		else
			showArrow(head ars) ++ "," ++ compArrows(tail ars)
	compNodes (nodes digraph) ++ "\n" ++ compArrows (arrows digraph)

treeBFS :: Digraph -> Node -> [Node] -> Digraph
treeBFS digraph node discoveredNodes = do
	let g = Digraph [] []
	g

main :: IO ()
main = do
	let drecords = [Record i ("T"++ show i) | i <- [1..20]]
	let dnodes = [Node (name recd) recd | recd <- drecords]
	let darrows = []
	let digraph = Digraph dnodes darrows
	let start = head dnodes
	let res = treeBFS digraph start []
	
	print (showDg digraph)