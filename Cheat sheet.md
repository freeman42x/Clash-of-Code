# Cheat sheet

## Input

Apply function on first 2 elements of an array:
2 ^ 3
foldr1 (^) [2, 3]

Zip first input with 3rd and 4th input etc.:
zip.(repeat.head)<*>(drop 2)$["Your honor, my client..","2","Bla","Bla"]
or even better:
f(x:_:z)=zipWith g(repeat x)z")

# Snippets

Feed an IO String to a function taking a String:
putStrLn=<<ioString

# Shorter

map words
(words<$>)