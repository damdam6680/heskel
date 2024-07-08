f :: Double -> Double
f x | x < -10 = x^2
    | x < 0   = sin(x)
    | x <= 2  = sqrt(x)


factorial :: Int -> Int
factorial x | x == 0 = 1
            | x > 0 = x * factorial(x-1)
            | otherwise = error "nie mozna ujemnych"


binomialCoefficient :: Int -> Int -> Int

binomialCoefficient n k = factorial(n) `div` (factorial(k)*factorial(n-k))

factorial2 :: Int -> Int

factorial2 x | x == 1 || x == 0 = 1
             | x > 1 = x * factorial2(x-2)
             | otherwise = error  "nie mozna ujemnych"
             
divides :: Int -> Int -> Bool

divides n k = n `rem` k == 0

seq' :: Int -> Double
seq' n | n == 1 = 3
       | n == 2 = 4
       | n > 2 = 0.5*seq'(n-1) + 2*seq'(n-2)
       | otherwise = error "nie mozna ujemnych"
gcd' :: Int -> Int -> Int
gcd' n k | n == k = n
         | n > k = gcd' (n-k) k
         | otherwise = gcd' n (k-n)

(><) :: Int  -> Int -> Bool

a >< b = gcd' a b == 1

type Complex = (Double, Double)

(+.) :: Complex -> Complex -> Complex
(a, b) +. (c, d) = (a+b, c+d)
(-.) :: Complex -> Complex -> Complex
(a, b) -. (c,d) = (a-b, c-d)
(*.) :: Complex -> Complex -> Complex
(a, b) *. (c,d) = (a*b, c*d)

re (x, _) = x

im (_, y) = y

i = (0, 1)