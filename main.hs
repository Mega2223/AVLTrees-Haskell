
f x = if x == 0 || x == 1 then x else f(x-1) + f(x-2)  

main = do
	let v = [f x | x <- [1..10]]
	
	print (show v)
	