module Tree where

import List
import Maybe

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
size tree = case tree of
    Empty -> 0
    (Node _ [] _) -> 1
    (Node _ children  _) -> 1 + (List.sum <| List.map size children)

depth : Tree a -> Int
depth tree = case tree of
    Empty -> 0
    (Node _ children _) -> 1 + (List.foldl max 0 <| List.map depth children)

nextId : Tree a -> Int
nextId root = let
    maxId : Tree a -> Int
    maxId root = case root of
        Empty -> 0
        (Node _ [] id) -> id
        (Node _ children id) -> max id <| List.foldl max 0 <| List.map maxId children
    in maxId root + 1
