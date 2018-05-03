import Data.Vect


helper : (x : Vect m elem) -> (txs : Vect m (Vect len elem)) -> Vect m (Vect (S len) elem)
helper [] [] = []
helper (x :: xs) (y :: ys) = (x :: y) :: helper xs ys


transposeMat : Vect n (Vect m elem) -> Vect m (Vect n elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let txs = transposeMat xs in (zipWith (::) x txs)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys




h2 : Num e => Vect n (Vect m e) -> Vect m e -> Vect n e
h2 [] ys = []
h2 (x :: xs) ys = sum (zipWith (*) x ys) :: h2 xs ys

h1 : Num e => (m1 : Vect n (Vect m e)) -> (tm2 : Vect p (Vect m e)) -> Vect p (Vect n e)
h1 [] [] = []
h1 (x :: xs) [] = []
h1 [] (x :: xs) = [] :: h1 [] xs
h1 m (y :: ys) = h2 m y :: h1 m ys

multMatrix : Num e => Vect n (Vect m e) -> Vect m (Vect p e) -> Vect n (Vect p e)
multMatrix [] _ = []
multMatrix m1 m2 = let tm2 = transposeMat m2 in transposeMat (h1 m1 tm2)

