
data Node = Node {left :: Node, right :: Node, val :: Int} | Void

printTree :: Node -> Int -> String
printTree (Node l r v) c = do
	let br = ['>' | _ <- [1..c]]
	show v ++ "\n" ++ br ++ printTree l (c+1) ++ "\n" ++ br ++ printTree r (c+1)
printTree Void _ = do
	"[ ]"

main :: IO ()
main = do
	let v = Void
	let a = Node v v 0
	let b = Node a v 1
	let c = Node v v 1
	let d = Node c b 2
	let e = Node v v 3
	let f = Node e d 6
	let g = Node f v 8
	let h = printTree g 0

	putStrLn h
	print "?"