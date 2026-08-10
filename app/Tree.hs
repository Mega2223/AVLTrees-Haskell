import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

data Node = Node {left :: Node, right :: Node, val :: Int} | Void

instance Show Node where
	show n = show (val n)

printTree :: Node -> Int -> String
printTree (Node l r v) c = do
	let br = concat ["|-" | _ <- [1..c]]
	show v ++ "\n" ++ br ++ "L: " ++ printTree l (c+1) ++ "\n" ++ br ++ "R: " ++ printTree r (c+1)
printTree Void _ = do
	"N"

populateMap :: Node -> Int -> Map.Map Int [Node] -> Map.Map Int [Node]
populateMap Void _ lvlMap = lvlMap
populateMap (Node l r v) nodeLvl lvlMap = do
	let node = Node l r v
	let lvListMaybe = Map.lookup nodeLvl lvlMap
	let llist = case lvListMaybe of
		(Just lllist) -> lllist
		Nothing -> []
	let nlist = llist ++ [node]
	let m = Map.insert nodeLvl nlist lvlMap
	let m1 = populateMap l (nodeLvl + 1) m
	let m2 = populateMap r (nodeLvl + 1) m1
	m2

printTreeH :: Node -> String
printTreeH Void = "N"
printTreeH (Node l r v)  = do
	let node = Node l r v
	let lmap = populateMap node 1 Map.empty
	let keys = Set.toAscList (Map.keysSet lmap)

	let lvlToStr lvl = do
		let lvLs = Map.lookup lvl lmap
		case lvLs of
			Nothing -> "?"
			(Just somelvl) -> foldl (\a b -> show a ++ " " ++ show b) "" somelvl
	let lvls = map lvlToStr keys
	foldl (\a b -> a ++ "\n" ++ b) "" lvls

flatten :: Node -> [Node]
flatten Void = []
flatten (Node l r v) = flatten l ++ [Node l r v] ++ flatten r

insertNoBalance :: Node -> Node -> Node
insertNoBalance Void node = node
insertNoBalance (Node ll rr vv) node = do
	if val node < vv then
		Node (insertNoBalance ll node) rr vv
	else
		Node ll (insertNoBalance rr node) vv

height :: Node -> Int
height Void = 0
height (Node ll rr _) = 1 + max (height ll) (height rr)

balance :: Node -> Int
balance Void = 0
balance (Node ll rr _) = height ll - height rr

rotateR :: Node -> Node
rotateR Void = Void
rotateR (Node ll rr vv) = do
	let x = Node ll rr vv
	let y = ll
	let a = left y
	let b = right y
	let c = rr
	Node a (Node b c vv) (val y)

rotateL :: Node -> Node
rotateL Void = Void
rotateL (Node ll rr vv) = do
	let x = Node ll rr vv
	let y = rr
	let c = ll 
	let b = left y
	let a = right y
	Node (Node c b (val x)) a (val y)

rotateLR :: Node -> Node
rotateLR Void = Void
rotateLR (Node ll rr vv) = do
	let x = Node ll rr vv
	let y = left x
	let z = right y
	let yy = rotateL y
	let xx = rotateR (Node yy (right x) (val x))
	xx

rotateRL :: Node -> Node
rotateRL Void = Void
rotateRL (Node ll rr vv) = do
	let x = Node ll rr vv
	let y = right x
	let z = left y
	let yy = rotateR y
	let xx = rotateL (Node (left x) yy (val x))
	xx

rebalance :: Node -> Node
rebalance Void = Void
rebalance (Node ll rr vv) = do
	let selfN = Node ll rr vv

	let llr = rebalance ll
	let rrl = rebalance rr
	let nn = Node llr rrl vv

	let bal = balance selfN
	if bal < -1 then
		if balance nn <= 0 then
			rotateL nn
		else
			rotateRL nn	
	else if bal > 1 then
		if balance nn >= 0 then
			rotateR nn
		else
			rotateLR nn	
	else
		nn

insertKeepBalance :: Node -> Node -> Node
insertKeepBalance Void node = node
insertKeepBalance (Node ll rr vv) node = do
	let node' = insertNoBalance (Node ll rr vv) node
	rebalance node'

main :: IO ()
main = do
	let insLeft node count = do
		let minval = head (flatten node)
		if count <= 0 then
			node
		else
			insLeft (insertKeepBalance node (Node Void Void (val minval - 1))) (count - 1)
			

	let t0 = insertKeepBalance Void (Node Void Void 20)
	let t = insLeft t0 20
	
	let h = printTree t 1

	putStrLn ("mm:= " ++ printTreeH t)
	putStrLn h
	print (show (flatten t))