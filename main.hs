import Control.Parallel (par)

-- This is effectively a reimplementation of the "map" function, but 
-- we get to utilize Haskell's neat "sparks", thus allowing for fast and efficient
-- multithreading, mostly handled by the runtime. 
-- Copied from https://github.com/cyga/real-world-haskell/blob/master/ch24/ParMap.hs 
parMap :: ( a -> b) -> [a] -> [b]
parMap f (x:xs) = let r = f x
                  in r `par` r : parMap f xs
parMap _ _ = []

-- This is the actual logic to check for Fizzbuzz.  
-- Basically the rules for fizzbuzz are "if it is
-- divisible by 3, write "fizz", if it's divisible
-- 5, write "buzz", if it's divisible by 3 and 5, 
-- write fizzbuzz, else just spit out the number. 
fizzBuzz :: (Integral a, Show a) => a -> String
fizzBuzz x | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
           | x `mod` 3 == 0 = "Fizz"
           | x `mod` 5 == 0 = "Buzz"
           | otherwise = show x

-- We'll partially apply the fizzBuzz function to quickly 
-- generate a helper function. 
parallelFizzMap = parMap fizzBuzz

-- This is just a quick helper function 
-- to concat two strings together. 
concatWithComma :: String -> String -> String
concatWithComma x y = x ++ "," ++ y

-- Again, using partial application, we can 
-- quickly generate a helper function. 
-- 
-- The "" at the end is there to give some
-- initial value and to make the types line up. 
makeList = foldr concatWithComma ""

main :: IO ()
main = putStrLn $ makeList . parallelFizzMap $ [1..100]
