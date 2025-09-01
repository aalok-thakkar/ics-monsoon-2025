

{-
  Now we turn to an unrelated, but still ancient problem: Computing square 
  roots. We will start with finding integer square roots.

  Given a non-negative integer N, we want to find its square root. However, 
  N may not be a perfect square. In this case, I want to find the integral part
  of the square root of N. How would you describe it mathematically?











  Given N, find r such that: 
  (i) r*r <= N
  (ii) (r+1)*(r+1) > N

-}

isqrt :: Int -> Int
-- Requires: n >= 0
-- Ensures: returns r where r*r <= n and (r+1)*(r+1) > n
isqrt n
  | n < 0     = error "square root requires n >= 0"
  | n == 0    = 0
  | otherwise = isqrtHelper 0 n






isqrtHelper :: Int -> Int -> Int
-- Requires: n >= 0 and guess >= 0 and guess*guess <= n
-- Ensures: returns r where r*r <= n and (r+1)*(r+1) > n
isqrtHelper guess n
  | guess * guess <= n = isqrtHelper (guess + 1) n
  | otherwise = guess - 1













{-

However, we may not want just an integer square root, but an approximation of 
the square root as a floating point. The Babylonians developed a remarkably 
accurate method for this, which iteratively updates the guess.

Their method, which we now call the Babylonian method (also found in the work
of Hero of Alexandria, and the Bakshali Manuscript), works as follows:

To find √N:
1. Make an initial guess x
2. Calculate a better guess as average of x and N/x
3. Repeat until desired accuracy

For example, to find √10:
Initial guess: 3
Next guess: (3 + 10/3)/2 = 3.167
Next: (3.167 + 10/3.167)/2 = 3.162...
And so on, converging to √10 ≈ 3.162278

Why does it work?
-}





divAverageSqrt :: Int -> Double
-- Requires: n >= 0
-- Ensures: result r approximates √n within floating point precision
divAverageSqrt n
  | n < 0     = error "square root requires n >= 0"
  | n == 0    = 0
  | otherwise = divAverageSqrtHelper (fromIntegral n) 1

-- Requires: n >= 0 and x >= 0
-- Ensures: result r approximates √n within floating point precision
divAverageSqrtHelper :: Double -> Double -> Double 
divAverageSqrtHelper n x
  | abs (x*x - n) < 0.000001 = x
  | otherwise                = divAverageSqrtHelper n ((x + n/x) / 2)


{-
These ancient algorithms demonstrate remarkable mathematical insight. We've 
implemented them in Haskell, taking advantage of its pattern matching and 
recursion.

Exercises:
1. Try implementing all of them in OCaml
2. Try to prove their correctness: That is, if requires condition holds for the 
    inputs, then the ensures condition must hold for the output. 
-}
