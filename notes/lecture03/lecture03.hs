-- Lecture 3: CS as a Human Endeavour

{-
Computer Science often feels like a thoroughly modern discipline, born in the 
age of silicon and digital revolution. Yet it stands on the shoulders of 
centuries of human intellectual achievement.

It draws on many other disciplines, including mathematics, philosophy, and 
literature. The very concept of computation itself was explored by ancient
Indian and Persian mathematicians through their work on algorithms.

Even our approach to problem solving draws heavily from philosophy. The 
systematic of logic and reasoning was developed by the Greeks and refined 
through the ages inform how we structure programs and prove their correctness. 

Literature too has played its role. The ideas of creativity and imagination are
at the heart of computer science. Consider for example, William Shakespeare’s 
The Tempest. In Prospero, we meet the prototype of a scholar whose power 
derives not from innate ability but from meticulous study and the precise 
application of knowledge. Caliban presents the perfect counterpoint—a being 
with extensive knowledge but no true understanding. When Prospero teaches him 
language, Caliban learns the syntax perfectly but misses entirely the deeper 
structures of meaning, purpose, and appropriate use. His bitter complaint 
reveals the dangerous gap between knowing and understanding:

"You taught me language, and my profit on ’t
Is I know how to curse. The red plague rid you
For learning me your language!"

— I.ii.363-365

Here lies the central lesson for computer scientists: Caliban can execute the 
language flawlessly, but he uses this perfect technical capability for purposes 
that completely subvert his teacher’s intent. He has mastered the interface 
while remaining utterly ignorant of the underlying logic, the ethical 
framework, the intended use cases. 

This dynamic has become urgently relevant in our age of AI. The programmer who 
depends on such tools without developing their own understanding risks becoming 
Caliban: technically proficient but strategically helpless, capable of perfect 
execution but incapable of wise judgment.

In this course, we focus on understanding over syntax. Last week, we programmed 
in OCaml. The same functions can also be written in other programming 
languages. For example, here they are in Haskell: 

Here, the type signature is omitted for brevity. The type Nat is defined as 
an inductive data type with two constructors, Zero and Succ. The function plus 
is defined recursively, with two cases: when the first argument is Zero, it 
returns the second argument, and when the first argument is Succ k, it returns 
Succ (plus k n).
-}


{-

let is_odd (n : int) : bool = 
    match (n mod 2) with 
    | 0 -> true
    | _ -> false
-}

isOdd :: Int -> Bool
isOdd n = case (mod n 2) of
    0 -> True 
    _ -> False

{-
let collatz (n : int) : int = 
    match (is_odd n) with 
    | true -> 3 * n + 1
    | false -> n / 2
-}

collatz :: Int -> Int
collatz n = case isOdd n of
    True -> 3 * n + 1
    False -> n `div` 2


{-
type nat = Zero | Succ of nat

let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n) 
-}

data Nat = Zero | Succ Nat deriving (Eq)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ k) n = Succ (plus k n)








-- How do we define multiplication?

{- 
   Multiplication can be defined recursively as follows:
   - For any natural number n, multiplying by Zero gives Zero.
   - For any natural number n, multiplying by Succ k gives n + (n * k). -}
   












mult :: Nat -> Nat -> Nat
mult a b = case a of
  Zero -> Zero
  Succ k -> plus (mult k b) b

mult2 :: Nat -> Nat -> Nat
mult2 Zero b = Zero
mult2 (Succ k) b = plus (mult k b) b


{-

three = S.S.S.Z
four  = S.S.S.S.Z

  mult2 three four 
= mult2 S.S.S.Z  S.S.S.S.Z
= plus (mult S.S.Z S.S.S.S.Z) S.S.S.S.Z
= plus (plus (mult S.Z S.S.S.S.Z) S.S.S.S.Z) S.S.S.S.Z
= plus (plus (plus (mult Z S.S.S.S.Z) S.S.S.S.Z) S.S.S.S.Z) S.S.S.S.Z
= plus (plus (plus Z S.S.S.S.Z) S.S.S.S.Z) S.S.S.S.Z
= plus (plus S.S.S.S.Z S.S.S.S.Z) S.S.S.S.Z

-}

-- Exercise: Write it in OCaml
-- Exercise: Try implementing exponentiation



{- 

    Recursion is a fundamental concept in mathematics. It is the default mode 
    of computation in functional languages, where we define operations in terms
    of themselves on smaller inputs.

   Many mathematical operations are defined recursively:

   - Addition: n + 0 = n
               n + Succ(k) = Succ(n + k)

   - Multiplication: n * 0 = 0  
                     n * Succ(k) = n + (n * k)

   - Exponentiation: n ^ 0 = 1
                     n ^ Succ(k) = n * (n ^ k)

   - Factorial: 0! = 1
                n! = n * (n-1)!

   This mirrors how we naturally think about these operations:
   - 3 * 4 is really just adding 3 to itself 4 times
   - 2^3 is multiplying 2 by itself 3 times
   - 5! is multiplying all numbers from 1 to 5
-}


-- Defining recursive functions on Int and Nat types:

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)


factNat :: Nat -> Nat
factNat Zero = Succ Zero
factNat (Succ n) = mult (Succ n) (factNat n)


-- Both of them are incorrect. Why?


















factInt :: Int -> Int
-- Requires n >= 0
factInt 0 = 1
factInt n = n * factInt (n - 1)

-- Safe factorial using safe multiplication
safeFact :: Int -> Int
-- Requires n >= 0
safeFact 0 = 1
safeFact n 
  | n < 0     = error "factorial requires n >= 0"
  | otherwise = n * safeFact (n - 1)



{- 
let rec factorial (n : int) : int = 
  match n with 
  | 0            -> 1
  | _ when n < 0 -> raise "error"
  | _            -> n * factorial (n - 1)
-} 


safeFact2 :: Int -> Int
-- Requires n >= 0
safeFact2 n 
  | n < 0     = error "factorial requires n >= 0"
  | otherwise = factInt n



-- Still incorrect, why?










-- Safe multiplication that checks for overflow
safeMult :: Int -> Int -> Int
safeMult 0 _ = 0
safeMult _ 0 = 0
safeMult a b
  | a > 0 && b > 0 && b > maxBound `div` a = error "multiplication overflow"
  | a < 0 && b < 0 && b < minBound `div` a = error "multiplication overflow" 
  | a > 0 && b < 0 && b < minBound `div` a = error "multiplication overflow"
  | a < 0 && b > 0 && b > maxBound `div` (-a) = error "multiplication overflow"
  | otherwise = a * b










correctFact :: Int -> Int
-- Requires n >= 0
correctFact 0 = 1
correctFact n 
  | n < 0     = error "factorial requires n >= 0"
  | otherwise = safeMult n (correctFact (n - 1))



{-
Note: Haskell (and many other languages, including OCaml) allows unsafe 
multiplication by default for performance and simplicity. Languages like Rust 
prevent this at compile time through overflow checks in debug mode. We don't 
write safe multiplication checks everywhere because:
-- 1. It adds runtime overhead
-- 2. Most calculations work within bounds
-- 3. Many applications can tolerate occasional overflow
-}







-- Exercise: Define triangular numbers










triangular :: Int -> Int
-- Requires n >= 0
triangular 0  = 0
triangular n 
  | n < 0     = error "triangular requires n >= 0"
  | otherwise = n + triangular (n-1)












{- 
We can also write it using the formula n * (n+1) `div` 2, but the recursive 
definition better shows the mathematical concept
-}

triangular' :: Int -> Int
-- Requires n >= 0
triangular' n = n * (n+1) `div` 2






{-
When is any implementation of triangular correct? 

Requires: n >= 0
Ensures: triangular n = n * (n+1) `div` 2
-}



{- 
 Problem: A Problem of Counting Meters


Pingala's Chhandasutra (c. 200 BCE) is one of the oldest known works on 
prosody. It systematically describes meters used in Sanskrit poetry.

  A meter consists of syllables that are either:
  - laghu (short syllable)
  - guru (long syllable)


  For example: 

    रात्रौ यथा मेघघनान्धकारे
    विद्युत्क्षणं दर्शयति प्रकाशम् ।
    बुद्धानुभावेन तथा कदाचित्
    लोकेषु पुण्येषु मतिः क्षणं स्यात् ॥
    
    -- Bodhicaryavatara 1.5


     Meter: Indravajra
     G G L / G G L / L G L / G / G
     ‒ ‒ ⌣/ ‒ ‒ ⌣ / ⌣ ‒ ⌣/ ‒ / ‒ 
            

  Question: Given m laghu syllables and k guru syllables, in how many 
  different ways can we arrange them to form valid meters?

  For example, if m = 2 and k = 1:
  - L L G (laghu laghu guru)
  - L G L (laghu guru laghu) 
  - G L L (guru laghu laghu)

  So there are 3 possible meters with 2 laghu and 1 guru syllables.

  This is a combinatorial problem - we need to find all possible ways to 
  arrange m laghu and k guru syllables.

  Pingala says that the first syllable can either be:
  - laghu: in which case we need to arrange (m-1) laghu and k guru syllables
  - guru: in which case we need to arrange m laghu and (k-1) guru syllables
  
  This naturally leads to a recursive solution where:
  meters(m,k) = meters(m-1,k) + meters(m,k-1)
-}










countMeters :: Int -> Int -> Int
-- Requires n >= 0 and k >= 0
countMeters 0 _      = 1
countMeters _ 0      = 1
countMeters m k     
  | m < 0 || k < 0   = 0                   -- Invalid meter
  | otherwise        = countMeters (m-1) k + countMeters m (k-1)

{- Examples:
  countMeters 2 1 = 3  -- LLG, LGL, GLL
  countMeters 1 1 = 2  -- LG, GL
  countMeters 2 0 = 1  -- LL
-}


-- Solving it using factorial:

{- 
  The number of ways to arrange m laghu and k guru syllables is equal to
  the number of ways to choose m positions out of (m+k) total positions
  for the laghu syllables (or equivalently, k positions for guru syllables).
  
  This is given by the binomial coefficient formula:
  C(m+k,m) = (m+k)! / (m! * k!)
-}

choose :: Int -> Int -> Int
-- Requires n >= 0 and k >= 0
choose n k = (fact n) `div` ((fact k) * (fact (n-k)))


-- We can verify that countMeters gives the same result as choose:

countMetersFactorial :: Int -> Int -> Int
-- Requires n >= 0 and k >= 0
countMetersFactorial m k = choose (m + k) m


-- Exercise: Prove using induction that countMeters and countMetersFactorial give the same result.









chooseRecursive :: Int -> Int -> Int
-- Requires n >= 0 and k >= 0
-- Ensures chooseRecursive n k = choose n k
chooseRecursive n k 
 | (n == k || k == 0) = 1
 | otherwise          = chooseRecursive (n-1) (k-1) + chooseRecursive (n-1) k




-- Problem: Enumerating the number of meters

{- 
  The meru prastara is a triangular arrangement showing the number of meters
  with a given number of laghu and guru syllables.
  
  For example:
               1             -- 0 syllables
             1   1           -- 1 syllable (1L, 1G)
           1   2   1         -- 2 syllables (1LL, 2LG, 1GG)
         1   3   3   1       -- 3 syllables (1LLL, 3LLG, 3LGG, 1GGG)
       1   4   6   4   1     -- 4 syllables
  
  Each number represents how many meters exist with that many L and G syllables.
  Each number is the sum of the two numbers above it.
-}



meru :: Int -> Int -> IO ()
meru n i 
  | i > n = return ()
  | otherwise = do
      putStr (replicate (n-i) ' ')
      printRow i 0
      putStrLn ""
      meru n (i+1)

printRow :: Int -> Int -> IO ()
printRow n k
  | k > n = return ()
  | otherwise = do
      putStr (show (chooseRecursive n k) ++ " ")
      printRow n (k+1)


main :: IO ()
main = meru 9 0




-- Problem: A Problem of Points



{-
  This famous problem emerged in 1654 when Antoine Gombaud, Chevalier de Méré, 
  a French nobleman and gambling enthusiast, posed it to Blaise Pascal. The 
  Chevalier had been puzzled by certain gambling probabilities and sought 
  mathematical clarity. Pascal, intrigued by the problem, began a 
  correspondence with Pierre de Fermat.
  
  Their exchange of letters discussing this problem became one of the most 
  important early developments in probability theory. The specific problem 
  arose from a popular dice game in the gambling salons of Paris. When games 
  had to be ended prematurely due to external circumstances (like the gambling
  house closing time), there were often disputes about how to fairly divide 
  the stakes.

  Situation:
  - Two players are playing a fair coin-flip game (or dice game).
  - First to win N (non-consecutive) rounds gets the stake.
  - But what if the game is stopped early?
  - How should the prize be fairly divided based on the score?


  
  Idea 1: Pacioli's Solution (1494)
  - Simply divide stakes in proportion to rounds won
  - Ignores number of rounds needed to win
  - Example: If A has won 2 rounds and B has won 1 round, A gets 2/3 and B gets 1/3

 -}

pacioliDivision :: Int -> Int -> (Double, Double)
-- requires: aScore and bScore are non-negative integers
-- ensures: proportional division of stakes
pacioliDivision aScore bScore = 
  let total = fromIntegral (aScore + bScore)
      aShare = (fromIntegral aScore) / total
  in (aShare, 1.0 - aShare)


{- 
pacioliDivision: int -> int -> float * float
let pacioliDivision' aScore bScore =
     let total = float_of_int (aScore + bScore) in
     let aShare = (float_of_int aScore) /. total in
     let bShare = 1.0 -. aShare in
     (aShare, bShare) 
     
-} 


-- What are the limitations?




{-      
  Idea 2: Tartaglia's Solution (1556)
  - Consider the size of lead and length of game
  - Adjust from equal division (1/2) based on lead/total
  - Example: If A leads by 1 in a 3-round game, A gets 2/3 and B gets 1/3

-}











tartagliaDivision :: Int -> Int -> Int -> (Double, Double)
-- requires: aScore, bScore, and gameSize are non-negative integers
--           aScore and bScore are less than or equal to gameSize
-- ensures: proportional division of stakes
tartagliaDivision aScore bScore gameSize =
     let lead = fromIntegral (aScore - bScore)
         total = fromIntegral gameSize
         aShare = 0.5 + (lead / total)
     in (aShare, 1.0 - aShare)



-- This is better, but what are the limitations?








{-
  Idea 3: Pascal's Solution (1654)
  - Count all possible future game sequences
  - Calculate probability of each player winning
  - Divide stakes according to these probabilities

That is, suppose player A needs m more wins and player B needs k more wins to 
reach the target. Then the game will end in at most (m + k - 1) tosses. The 
number of ways A can win is equal to choosing m wins or more out of the first 
(m + k - 1) tosses.


Win and lose are binary, like laghu and guru. How many ways are there to make a
meter with n syllables such that it has at least m laghu?
-}


sumChoose :: Int -> Int -> Int
-- Requires m >= 1 and k >= 1
sumChoose m k 
  | m > total = 0
  | otherwise = chooseRecursive total m + sumChoose (m + 1) k
  where total = m + k - 1











pascalDivision :: Int -> Int -> Int -> (Double, Double)
--  Requires:
--   - target > aScore (A hasn't won yet)
--   - target > bScore (B hasn't won yet) 
--   - aScore >= 0 and bScore >= 0 (non-negative scores)
--  Ensures: proportional division of stakes
pascalDivision aScore bScore target = 
    let m = target - aScore
        k = target - bScore
        total = m + k - 1
        denominator = 2 ^ total 
        aShare = fromIntegral (sumChoose m k) / fromIntegral denominator
        bShare = 1.0 - aShare
    in (aShare, bShare)



  -- This exchange between Pascal and Fermat is considered the birth of modern probability theory.



