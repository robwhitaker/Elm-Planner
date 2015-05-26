module Tree where

import List
import Utils

type Tree a = Empty | Node a (List (Tree a)) Int
type ShiftDirection = Up | Down
type NodeMovement = ShiftUp | ShiftDown | Lift | Lower

empty : Tree a
empty = Empty

newNode : Tree a -> a -> (List (Tree a)) -> Tree a
newNode root value children = Node value children (nextId root)

dummyNode : a -> Tree a
dummyNode value = Node value [] -1 

children : Tree a -> List (Tree a)
children (Node _ children _) = children

value : Tree a -> a
value (Node value _ _) = value

id : Tree a -> Int
id (Node _ _ id) = id

parent : Tree a -> Tree a -> Maybe (Tree a)
parent child root  = if (children root) == [] then Nothing
                     else if List.member child (children root) then Just root
                     else List.head <| List.filterMap (parent child) (children root)

valueM : (a -> a) -> Tree a -> Tree a
valueM f (Node value children id) = Node (f value) children id

map : (a -> b) -> Tree a -> Tree b
map f (Node value children id) = Node (f value) (List.map (map f) children) id

mapToNodeById : (a -> a) -> Int -> Tree a -> Tree a
mapToNodeById f id (Node value children id') = 
    if id == id' then Node (f value) children id' else Node value (List.map (mapToNodeById f id) children) id'

mapN : (Tree a -> Tree a) -> Tree a -> Tree a
mapN f (Node value children id) = f (Node value (List.map (mapN f) children) id)

addChild : Tree a -> Tree a -> Tree a
addChild child (Node pVal pChildren id) = Node pVal (pChildren ++ [child]) id

size : Tree a -> Int
size tree = fold (\node acc -> acc + 1) 0 tree

depth : Tree a -> Int
depth tree = case tree of
    Empty -> 0
    (Node _ children _) -> 1 + (List.foldl max 0 <| List.map depth children)

nextId : Tree a -> Int
nextId root = 1 + foldN (\(Node _ _ id) acc -> if id > acc then id else acc) -1 root

flatten : Tree a -> List (Tree a)
flatten tree = case tree of
    Empty               -> []
    (Node _ [] _)       -> [tree]
    (Node _ children _) -> [tree] ++ (List.concatMap flatten children)

foldN : (Tree a -> b -> b) -> b -> Tree a -> b
foldN f acc tree = case tree of
    Empty               -> acc
    (Node _ [] _)       -> f tree acc
    (Node _ children _) -> List.foldl f acc (flatten tree)

fold : (a -> b -> b) -> b -> Tree a -> b
fold f acc tree = case tree of
    Empty -> acc
    (Node value [] _) -> f value acc
    (Node value children _) -> let 
        newAcc = f value acc
        in List.foldl (flip (fold f)) newAcc children 

nodeById : Int -> Tree a -> Maybe (Tree a)
nodeById id tree = List.head <| List.filter (\(Node _ _ id') -> id == id') <| flatten tree

nodeByIdWithDefault : Tree a -> Int -> Tree a -> Tree a
nodeByIdWithDefault default id tree = Maybe.withDefault default <| nodeById id tree

addChildTo : Tree a -> Tree a -> Tree a -> Tree a
addChildTo parent child root = 
    if parent == root 
    then addChild child parent
    else Node (value root) (List.map (addChildTo parent child) (children root)) (id root)

addChildToNodeById : Int -> Tree a -> Tree a -> Tree a
addChildToNodeById id child root = case nodeById id root of
    Nothing     -> root
    Just parent -> addChildTo parent child root

removeNode : Tree a -> Tree a -> Tree a
removeNode node root = 
    if List.member node (children root) 
    then Node (value root) (List.filter ((/=) node) (children root)) (id root)
    else Node (value root) (List.map (removeNode node) (children root)) (id root) 

removeNodeById : Int -> Tree a -> Tree a
removeNodeById id root = removeNode (Maybe.withDefault (dummyNode (value root)) <| nodeById id root) root

shiftNode : ShiftDirection -> Tree a -> Tree a -> Tree a
shiftNode dir node root = 
    if List.member node (children root)
    then let
        offset = if dir == Up then -1 else 1
        indexedChildren = List.indexedMap (,) (children root)
        targetNode = Maybe.withDefault (-1, node) <| List.head <| List.filter ((==) node << snd) indexedChildren
        newChildren = (List.map snd << 
                       List.sortBy fst << 
                       List.map (\node' -> if node' == targetNode then (fst node' + offset, snd node') else node') <<
                       List.map (\(index, node') -> if index == (fst targetNode + offset) then (index-offset,node') else (index,node'))) indexedChildren
        in Node (value root) newChildren (id root)
    else Node (value root) (List.map (shiftNode dir node) (children root)) (id root)

liftNode : Tree a -> Tree a -> Tree a
liftNode node root = let
    grandparent = root
    parents = children grandparent
    kids = List.map children parents
    parentsAndKids = List.map2 (,) parents kids
    parent = List.head <| List.map fst <| List.filter (snd >> List.member node) parentsAndKids
    in case parent of
        Just p  -> Node (value grandparent) 
                        ((Utils.takeWhile ((/=) p) parents) ++ [removeNode node p, node] ++ (List.drop 1 <| Utils.dropWhile ((/=) p) parents))
                        (id grandparent)
        Nothing -> Node (value root) (List.map (liftNode node) (children root)) (id root)

lowerNode : Tree a -> Tree a -> Tree a
lowerNode node root = 
    if List.member node (children root)
    then let
        nodePosition = (List.length (children root)) - (List.length <| Utils.dropWhile ((/=) node) (children root))
        elementBeforeNode = if nodePosition == 0 then Nothing else List.head <| List.drop (nodePosition-1) (children root)
        in case elementBeforeNode of
            Just elem -> Node (value root) 
                              (Utils.takeWhile ((/=) elem) (children root) ++ [addChild node elem] ++ (List.drop 1 <| Utils.dropWhile ((/=) node) (children root))) 
                              (id root)
            Nothing -> root
    else Node (value root) (List.map (lowerNode node) (children root)) (id root)


moveNode : NodeMovement -> Tree a -> Tree a -> Tree a
moveNode movement node root = case movement of
    ShiftUp     -> shiftNode Up node root
    ShiftDown   -> shiftNode Down node root
    Lift        -> liftNode node root
    Lower       -> lowerNode node root

moveNodeById : NodeMovement -> Int -> Tree a -> Tree a
moveNodeById movement id root = moveNode movement (Maybe.withDefault root <| nodeById id root) root
