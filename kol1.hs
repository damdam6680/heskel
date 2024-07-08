
f1 :: Double -> Double -> Double
f1 a h = (a * h) / 2

f2 :: Double -> Double -> Double
f2 = (*).(/2)


isHexByte :: String -> Bool

isHexByte [x, y] = hexDigit x && hexDigit y
isHexByte _ = False

hexDigit x = x `elem` ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

type Vector3 = (Double, Double, Double)

infixr 4 *.

(*.) :: Vector3 -> Vector3 -> Double
(x1, y1, z1) *. (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2


factors :: Int -> [Int]

factors n = aux n n

aux n 1 = [1]
aux n i | n `rem` i == 0 = i : aux n (i - 1)
        | otherwise = aux n (i - 1)

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            | Null

evenLeaf :: Integral a => Tree a -> [a]

evenLeaf Null = []
evenLeaf (Leaf a) | even a = [a]
                  | otherwise = []

evenLeaf (Node _ left right) = evenLeaf left ++ evenLeaf right



tree = Node 7 (Node 4 (Leaf 2) (Leaf 5)) (Leaf 10)





