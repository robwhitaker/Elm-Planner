module Tree where

import List
import Maybe

type Tree a = Empty | Node a (List (Tree a))

children : Tree a -> List (Tree a)
children (Node _ children) = children

value : Tree a -> a
value (Node value _) = value

valueM : (a -> a) -> Tree a -> Tree a
valueM f (Node value children) = Node (f value) children

map : (Tree a -> Tree a) -> Tree a -> Tree a
map f tree = case tree of
    (Node _ []) -> f tree
    (Node value children) -> f (Node value (List.map (map f) children))

addChild : Tree a -> Tree a -> Tree a
addChild child (Node pVal pChildren) = Node pVal (pChildren ++ [child])

size : Tree a -> Int
size tree = case tree of
    Empty -> 0
    (Node _ []) -> 1
    (Node _ children) -> 1 + (List.sum <| List.map size children)

depth : Tree a -> Int
depth tree = case tree of
    Empty -> 0
    (Node _ children) -> 1 + (List.foldl max 0 <| List.map depth children)
