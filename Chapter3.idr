import Data.Vect

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

ins : Ord elem => (x : elem) -> (xss : Vect len elem) -> Vect (S len) elem
ins x [] = [x]
ins x (y :: xs) = case x < y of
                       True => x :: y :: xs
                       False => y :: ins x xs


total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xss = insSort xs in ins x xss

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myMapL : (a -> b) -> List a -> List b
myMapL f [] = []
myMapL f (x :: xs) = f x :: myMapL f xs

myMapV : (a -> b) -> Vect n a -> Vect n b
myMapV f [] = []
myMapV f (x :: xs) = f x :: myMapV f xs

