import Data.Char
sumOfSquares :: [Double] -> Double
sumOfSquares xs = sum (map (^2) xs)

sumOfSquares' :: [Double] -> Double

sumOfSquares' [] = 0
sumOfSquares' (x:xs) = x + sumOfSquares' xs


countLower :: String -> Int

countLower [] = 0
countLower (x:xs) | (x >= 'a' && x <= 'z') = 1 + countLower xs
                  | otherwise = countLower xs
countUpper :: String -> Int

countUpper [] = 0
countUpper (x:xs) | (x >= 'A' && x <= 'Z') = 1 + countUpper xs
                  | otherwise = countUpper xs


countLowerUpper :: String -> (Int, Int)

countLowerUpper xs = (countLower xs, countUpper xs)

string2bools :: String -> [Bool]
string2bools xs = map(isLower) xs

cgtx :: Integer -> [Integer] -> Integer
cgtx _ [] = 0
cgtx w (x:xs) | x > w = 1 + cgtx w xs
              | otherwise = cgtx w xs

gtx :: Integer -> [Integer] -> [Integer]

gtx _ [] = []
gtx w (x:xs) | x > w = x : gtx w xs
             | otherwise = gtx w xs



