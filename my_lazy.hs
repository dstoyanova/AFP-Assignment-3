import Data.List

createOutList([], h:t, _, maxIndex) = 
    createOutList([h], t, 1, maxIndex)
createOutList(currentList, _, currentListLength, maxIndex) 
    | maxIndex < currentListLength = currentList
createOutList(currentList, h:t, currentListLength, maxIndex) = 
    createOutList(currentList ++ [h] ++ currentList, t, currentListLength * 2 + 1, maxIndex)

nth(0, l) = l
nth(index, h:t) = nth(index - 1, t)

getSum(0, h:t) = h
getSum(index, h:t) = h + getSum(index - 1, t)

primes :: [Integer]
primes = sieve [2..] 
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

lazy :: Integer -> Integer -> [Integer] -> Integer
lazy minIndex maxIndex inList = 
    getSum(maxIndex - minIndex, nth(minIndex - 1, createOutList([], inList, 0, maxIndex + 1))) 