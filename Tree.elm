module Tree where

import List

type Tree a = Empty | Node a (List (Tree a)) Int

empty : Tree a
empty = Empty

newNode : Tree a -> a -> (List (Tree a)) -> Tree a
newNode root value children = Node value children (nextId root)

children : Tree a -> List (Tree a)
children (Node _ children _) = children

value : Tree a -> a
value (Node value _ _) = value

id : Tree a -> Int
id (Node _ _ id) = id

valueM : (a -> a) -> Tree a -> Tree a
valueM f (Node value children id) = Node (f value) children id

map : (Tree a -> Tree a) -> Tree a -> Tree a
map f tree = case tree of
    (Node _ [] _) -> f tree
    (Node value children id) -> f (Node value (List.map (map f) children) id)

addChild : Tree a -> Tree a -> Tree a
addChild child (Node pVal pChildren id) = Node pVal (pChildren ++ [child]) id

size : Tree a -> Int
size tree = fold (\node acc -> acc + 1) 0 tree

depth : Tree a -> Int
depth tree = case tree of
    Empty -> 0
    (Node _ children _) -> 1 + (List.foldl max 0 <| List.map depth children)

nextId : Tree a -> Int
nextId root = 1 + fold (\(Node _ _ id) acc -> if id > acc then id else acc) -1 root

flatten : Tree a -> List (Tree a)
flatten tree = case tree of
    Empty -> []
    (Node _ [] _) -> [tree]
    (Node _ children _) -> [tree] ++ (List.concat <| List.map flatten children)

fold : (Tree a -> b -> b) -> b -> Tree a -> b
fold f acc tree = case tree of
    Empty               -> acc
    (Node _ [] _)       -> f tree acc
    (Node _ children _) -> List.foldl f acc (flatten tree)
