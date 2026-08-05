f :: Int -> Int
f x = if x == 0 || x == 1 then x else f(x-1) + f(x-2)  

treeFrom :: ([Char],[(Char,Char)]) -> ([Char],[(Char,Char)])
treeFrom graph = do
	let g = ([],[])
	g

main :: IO ()
main = do
	let v = [f x | x <- [1..30]]
	
	print (show v)
