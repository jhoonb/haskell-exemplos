{- 
	github/jhoonb/haskell-exemplos
	Mínimo multiplo comum entre dois números
	Autor: Jhonathan Paulo Banczek
	2014 - jhoonb.com
-}

-- teste deterministico simples, por
-- divisões sucessivas até raiz(a), onde a é o número que será testado. 

{-
primeiro valida os números verificando se os mesmos são 1,2,3 ou 5,
depois calcula se o número é multiplo de 2 ou 5 e retorna que é composto,
por último testa todos os números IMPARES de [3 até raiz de a], fazendo
modulo de a por x, caso seja == 0, adiciona o elemento na lista,
que por sua vez, se o TAMANHO da lista for == 0 significa que o número
não é divisivel por nenhum número exceto 1 e ele mesmo.

-}

-- função pouco lenta quando o número testado é muito grande

testePrimo :: Integer -> Bool
testePrimo a
    | a <= 1 = False
    | a == 2 = True
    | a == 3 = True
    | a == 5 = True
    | mod a 2 == 0 = False
    | mod a 5 == 0 = False 
    | length [ x | x <- lista, mod a x == 0] == 0 = True
    | otherwise = False
    where
        lista = [ i | i <- [3, 5 .. (floor (sqrt(fromIntegral a)))]]


-- função reescrita usando função all(), muito mais eficiente que a função acima

testePrimo2 :: Integer -> Bool
testePrimo2 a
    | a <= 1 = False
    | a == 2 = True
    | a == 3 = True
    | a == 5 = True
    | even a == True = False
    | mod a 5 == 0 = False 
    | not(any(\x -> mod a x == 0) [3, 5 .. (floor (sqrt(fromIntegral a)))]) = True == True
    | otherwise = False


-- Teste de Fermat, teste probabilistico, possui pseudo primos.

testeFermat :: Integer -> Bool
testeFermat a 
    | mod (2 ^ (a-1) ) a == 1 = True
    | otherwise = False


{- ------------------------- exemplos ----------------

GHCI> testePrimo 10 => False

GHCI> testePrimo 5734253 => False

GHCI> testePrimo2 17 => True

GHCI> testePrimo 57342673 => True

GHCI> testePrimo (-10008678678678) => False

GHCI> testePrimo2 573426733534534534535535545345345345345345345345345 => False
-}
