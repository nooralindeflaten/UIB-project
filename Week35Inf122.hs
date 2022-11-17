
{-
typevariabler
typeklasser.

f :: A -> B -> A
a :: A 
f a b

6. f :: A -> A og g :: (A -> A) -> A -> A 
g f
g f :: A -> A 
funksjonsargument. 

id :: t -> t
id a = a 

const :: a -> b -> a 
const a b = a

h :: (a -> a) -> a -> a
h z x = z (z x)
[] :: [a]
tom liste av alle typer. 

-}

f :: a -> a -> a
f x y = y

g :: a -> [[a]]
g x = [[x]]

t :: (a -> b) -> (b -> c) -> a -> c
t fx gx a = gx(fx a) 

s :: (a -> b -> c) -> (a -> b) -> a -> c
s fx gx a = fx a(gx a)
