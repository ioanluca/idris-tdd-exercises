-- 1
{- 
    ("A", "B", "C") - (String, String, String)
    ["A, "B", "C"] - List String
    (('A', 'B'), 'C') - ((Char, Char), Char)
-}

-- 2
palindrome : String -> Bool
palindrome x = let rx = reverse x in x == rx

-- 3 
palindrome' : String -> Bool
palindrome' x = let lx = toLower x
                    rx = reverse lx
                in lx == rx

-- 4 & 5
palindrome10 : Nat -> String -> Bool
palindrome10 n x = (x == reverse x) && (length x > n)

-- 6
counts : String -> (Nat, Nat) 
counts str = (length . words $ str, length str)

-- 7
top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort 

-- 8 
over_length : Nat -> List String -> Nat
over_length n = length . filter (> 3) . map length

-- 9

main : IO () 
main = repl "> " output
    where output : String -> String  
          output i = show (palindrome i) ++ " " ++ show (counts i) ++ "\n"
