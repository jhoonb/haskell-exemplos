{- 
	github/jhoonb/haskell-exemplos
	Mínimo multiplo comum entre dois números
	Autor: Jhonathan Paulo Banczek
	2014 - jhoonb.com
-}

-- Função máximo divisor comum entre de a,b

mdc :: Integral a => a -> a -> a
mdc a b | mod a b == 0 = b
        | mod b a == 0 = a
        | a > b = mdc b (mod a b)
        | a < b = mdc a (mod b a)


-- Função miínimo multiplo comum
-- formula: a * b / (mdc(a, b))

mmc :: Integral a => a -> a -> a
mmc a b | a == 0 = 0
        | b == 0 = 0
        | a == b = a
        | otherwise = div (a * b) (mdc a b) 

{- ------------------- Exemplos------------------------

GHCI> mmc 20 8 => 40

GHCI> mmc 100 0 => 0

GHCI> mmc 100 100 => 100

GHCI> mmc 12121212 988863 => 3995406020652

GHCI> mmc ( mmc 45 120 ) (mmc 120 75) => mmc 45, 120, 75 => 1800

-}
