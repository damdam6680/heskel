import Data.Char

f1 :: Fractional a => a -> a -> a
f1 a h = (a*h)/2
f2 :: Fractional a => a -> a -> a
f2 = (*).(/2)


byte2int :: String -> Int

byte2int [] = 0
byte2int (x:xs) | length xs > 1 = error "max 2 znaki"
                | otherwise = doc x * 16 ^ length xs + byte2int xs

doc x | elem x ['0'..'9'] = ord x - ord '0'
      | elem x ['a'..'f'] = ord x - ord 'a' + 10
      | elem x ['A'..'F'] = ord x - ord 'A' + 10
      | otherwise = error "Nieprawidlowy znak szesnastkowy"

type Complex = (Double, Double)

infixl 7 *.

(*.) :: Complex -> Complex -> Complex

(a, b) *. (c, d) = (a*c - b*d, a * d + b * c)

digits :: Integral  a => a -> Int
digits x | x < 0 = digits (-x)
         | x < 10    = 1
         | otherwise = 1 + digits (x `quot` 10)

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            | Null

testTree = Node 7 (Node 4 (Leaf 2) (Leaf 5)) (Leaf 10)

addOddNodes :: Integral a => Tree a -> a
addOddNodes Null = 0
addOddNodes (Leaf _) = 0
addOddNodes (Node a left right) | odd a = a + addOddNodes left + addOddNodes right
                                | otherwise = addOddNodes left + addOddNodes right

