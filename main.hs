mdc x y = if y == 0 then x 
else mdc y (mod x y)

palindrome2 x y | x==0 = y
                | otherwise = palindrome2 (div x 10) ((mod x 10)+10*y)

palindrome x | x == palindrome2 x 0 = True
             | otherwise = False

decimalToBinary x | x == 0 = x
                  | otherwise = (mod x 2) + 10*decimalToBinary (div x 2)

binaryToDecimal x | x == 0 = x
                  | otherwise = 2*binaryToDecimal (div x 10) + (mod x 10)


triangulo x y z | x == y && y == z = "Equilatero"
                | (x == y && x /= z) || (y == z && y /= x) || (z == x && z /= y) = "Isosceles"
                | otherwise = "Escaleno" 


(@@) :: Bool -> Bool -> Bool
False @@ False = False
_ @@ _ = True


fatorial 0 = 1
fatorial n = n * fatorial(n-1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci(n-1)  + fibonacci(n-2)

pow x 0 = 1
pow x y = x * pow x (y-1)

ult [x] = x
ult (_:xs) = ult xs

prim [x] = []
prim (x:xs) = x: prim xs

cauda3 [x] = [x]
cauda3 (_:xs) = xs

cauda1 x = if null x then []
        else drop 1 x

cauda2 x | null x = []
         | otherwise = drop 1 x

produto = \x -> (\y -> x * y)
pares = \n -> map (\x -> x*2)[0..n-1]
add = \x -> (\y -> x + y)
odds n = map (\x -> x*2+1)[1..n-1]

soma = (+1)
dobro = (*2)

metade :: Double -> Double

metade = (/2)

multiplos7um = \n -> [x*7 | x <-[1..n-1]]
multiplos7 = \n -> map (\x -> x*7)[0..n-1]

tabuada9 n = [x*n | x<-[0..9]]

indicesSup m n = [(x, y) | x <- [0..m-1], y <-[x..n-1]]
indicesInf m n = [(x, y) | x <- [0..m-1], y <-[0..x]]

indicesLinha m n = [(x, y) | x <- [0..m-1], y <- [0..n-1]]
indicesColuna m n = [(x, y) | y <- [0..n-1], x <- [0..m-1]]

lista :: [[t]] -> [t]

lista listas = [ y | x <- listas, y <- x]

geraImpar n = [ x | x <- [0..2*n] , odd x]

fatores n = [x | x <- [1..n], mod n x == 0]

prime n = fatores n == [1,n]

geraPrimo n = tak n [ x | x <- [2..], prime x]

tak 0 _ = []
tak _ [] = []
tak n (x:xs) = x: tak(n-1) xs

perfeitos n = n == sum (fatores n) - n 

perfeito n = [ x | x <- [1..n], perfeitos x]

fib x y = x: (fib y (x+y))

obtenha (x:xs) 1 = x
obtenha (x:xs) n = obtenha xs (n-1)

-- [1, 2, 3] + [4, 5, 6] = [(1,4), (2,5), (3,6)]
-- [1, 2, 3, 4] = [(1,2), (2,3), (3,4)]

zipa [] _ = []
zipa _ [] = []
zipa (x:xs) (y:ys) = (x,y): zipa xs ys

--[1,2,3,4] = zipa [1,2,3,4] [2,3,4]
-- zipa = [(1,2),(2,3),(3,4)]
--pairs (x:xs) = zipa (x:xs) xs
pairs x = zipa x (tail x)

--and nesse caso recebe uma lista de Bool, se 
-- [True, True, True] = True
-- [False, False, False] = False
-- [False, ... , ...] = False
sort xs = and [x<=y | (x,y) <- pairs xs]

mult = \xs -> \ys -> sum [i*j| (i,j) <- zipa xs ys]

numletras xs c =  sum [1| x <- xs, x == c]

posicoes x xs = [b| (a,b) <- zipa xs [0..length xs], x==a]

tripla n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n]  , x^2 + y^2 == z^2]