{- 
	github/jhoonb/haskell-exemplos
	Máximo divisor comum entre dois números (por divisões sucessivas)
	Autor: Jhonathan Paulo Banczek
	2014 - jhoonb.com
-}

mdc :: Integral a => a -> a -> a
mdc a b | mod a b == 0 = b
        | mod b a == 0 = a
        | a > b = mdc b (mod a b)
        | a < b = mdc a (mod b a)


{- --------------- exemplos --------------------

GHCI> mdc 12 6 => 6 

GHCI> mdc 15 20 => 5

GHCI> mdc 12 20 => 4

GHCI> mdc (mdc 12 20) (mdc 20 24) => mdc 12 20 24 => 4

GHCI> mdc (mdc (mdc 12 20) (mdc 20 24)) (mdc 24 40) 
											 => mdc 12 20 24 40 => 4

-}
