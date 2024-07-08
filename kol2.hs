import Data.Char

byte2int :: String -> Int

byte2int [] = 0

byte2int (x:xs) | length xs > 1 = error "max 2 znaki"
                | otherwise = doc x*16 ^ length xs + byte2int xs

dec x | elem x ['0'..'9'] = ord x - ord '0'
      | elem x ['a'..'z'] = ord x - ord 'a' + 10
      | elem x ['A'..'Z'] = ord x - ord 'A' + 10
      | otherwise = error "Nieprawidlowy znak szczesntokowy"