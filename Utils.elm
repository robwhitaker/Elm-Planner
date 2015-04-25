module Utils where
 
import List
 
{-| Take members from a list while a given condition is met. 
 
    takeWhile ((==) 1) [1,1,1,1,5,4,6,1] == [1,1,1,1]
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile f xs = case xs of
    (x::xs) -> if f x then x :: takeWhile f xs else []
    [] -> []
 
{-| Drop members from a list while a given condition is met. 
 
    dropWhile ((==) 1) [1,1,1,1,5,4,6,1] == [5,4,6,1]
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs = case xs of
    (x::xs) -> if f x then dropWhile f xs else (x::xs)
    [] -> []