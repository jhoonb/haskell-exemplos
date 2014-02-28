{- 
	github/jhoonb/haskell-exemplos
	sucessão de Fibonacci ou sequência de Fibonacci 
	Autor: Jhonathan Paulo Banczek
	2014 - jhoonb.com
-}

-- recursivo pouco eficiente
fib :: Int -> Int
fib n | n < 2 = n
      | otherwise = (fib (n - 1) ) + (fib (n - 2) )


{- --------------- exemplos --------------------

GHCI> fib 10 => 55

GHCI> fib 30 => 832040

GHCI> fib 33 => 3524578

-}
