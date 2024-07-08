data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            | Null

testTree = Node 7 (Node 3 (Leaf 2) (Leaf 5)) (Leaf 10)

gtx4tree :: Ord a => a -> Tree a -> [a]
gtx4tree x Null = []
gtx4tree x (Leaf a)
    | a > x = [a]
    | otherwise = []
gtx4tree x (Node a left right)
    | a > x = a : rest
    | otherwise = rest
  where
    rest = gtx4tree x left ++ gtx4tree x right
    
cgtx4leafs :: (Num b, Ord a) => a -> Tree a -> b
cgtx4leafs x Null = 0
cgtx4leafs x (Leaf a)
    | a > x     = 1
    | otherwise = 0
cgtx4leafs x (Node a left right)
    | isLeaf left && isLeaf right = cgtx4leafs x left + cgtx4leafs x right
    | otherwise                   = cgtx4leafs x left + cgtx4leafs x right


isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False

gtx4leafs :: Ord a => a -> Tree a -> [a]
gtx4leafs x Null = []
gtx4leafs x (Leaf a)
    | a > x     = [a]
    | otherwise = []
gtx4leafs x (Node _ left right) = gtx4leafs x left ++ gtx4leafs x right

evenLeafs :: Integral a => Tree a -> [a]
evenLeafs Null = []
evenLeafs (Leaf a) | even a = [a]
                   | otherwise = []

evenLeafs (Node _ left right) = evenLeafs left ++ evenLeafs right


addOddNodes :: Integral a => Tree a -> a
addOddNodes Null = 0
addOddNodes (Leaf _) = 0  -- Ignorujemy warto ci li ci, zgodnie z za o eniami.
addOddNodes (Node a left right)
    | odd a     = a + addOddNodes left + addOddNodes right  -- Sumujemy, je li warto   w z a jest nieparzysta.
    | otherwise = addOddNodes left + addOddNodes right      -- Kontynuujemy przeszukiwanie, je li warto   jest parzysta.
