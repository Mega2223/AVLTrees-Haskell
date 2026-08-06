data Node = Node {left :: Node, right :: Node, val :: Int} | Void

printTree :: Node -> Int -> String
printTree (Node l r v) c = do
	let br = concat ["|-" | _ <- [1..c]]
	show v ++ "\n" ++ br ++ "L: " ++ printTree l (c+1) ++ "\n" ++ br ++ "R:" ++ printTree r (c+1)
printTree Void _ = do
	"N"

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

	let bal = balance selfN

	let llr = rebalance ll
	let rrl = rebalance rr
	
	selfN

insertKeepBalance :: Node -> Node -> Node
insertKeepBalance Void node = node
insertKeepBalance (Node ll rr vv) node = do
	let node' = insertNoBalance (Node ll rr vv) node

	node'

-- TODO função printflat

main :: IO ()
main = do
	let t0 = insertNoBalance Void (Node Void Void 9)
	let t1 = insertNoBalance t0 (Node Void Void 8)
	let t2 = insertNoBalance t1 (Node Void Void 7)
	let t3 = insertNoBalance t2 (Node Void Void 6)
	let t4 = insertNoBalance t3 (Node Void Void 5)
	let t5 = insertNoBalance t4 (Node Void Void 4)
	
	let h = printTree t5 1

	putStrLn h
	print "?"