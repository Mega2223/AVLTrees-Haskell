
data Node = Node {left :: Node, right :: Node, val :: Int} | Void

printTree :: Node -> Int -> String
printTree (Node l r v) c = do
	let br = concat ["|-" | _ <- [1..c]]
	show v ++ "\n" ++ br ++ "L: " ++ printTree l (c+1) ++ "\n" ++ br ++ "R:" ++ printTree r (c+1)
printTree Void _ = do
	"N"

insertNoBalance :: Node -> Node -> Node
insertNoBalance (Node ll rr vv) node = do
	if val node < vv then
		Node (insertNoBalance ll node) rr vv
	else
		Node ll (insertNoBalance rr node) vv
insertNoBalance Void node = node

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