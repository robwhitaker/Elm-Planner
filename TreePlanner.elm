module TreePlanner where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Signal
import Signal exposing (Signal)
import Window

import Tree as T
import Tree exposing (Tree, NodeMovement)

---------- MODEL ----------

-- Application state
type alias State = {
    rootNode : Tree Item,
    projectTitle : String,
    sidebarSize : Float
}

type alias Item = {
    title : String,
    content : String,
    expanded : Bool,
    selected : Bool,
    renaming : Bool
}

newItem : Item 
newItem = {
    title = "new",
    content = "",
    expanded = False,
    selected = True,
    renaming = False
    }

emptyModel : State
emptyModel = {
    rootNode = T.Node newItem [T.Node newItem [] 1, T.Node newItem [T.Node newItem [] 3] 2] 0, --T.newNode T.empty newItem [],
    projectTitle = "Untitled",
    sidebarSize = 0.2
    }

---------- UPDATE ----------

type Action 
    = NewItem Int
    | RenameItem String Int
    | UpdateItem String Int
    | RenamingItem Int
    | DeleteItem Int
    | MoveNode NodeMovement Int
    | ToggleExpanded Int
    | NoOp

update : Action -> State -> State
update action state = let 
    getNodeFromId = (Maybe.withDefault (T.dummyNode newItem) << flip T.nodeById state.rootNode)
    in case action of
        NewItem selectedId -> 
            { state | rootNode <- T.addChildTo (T.mapToNodeById (T.valueM (\item -> { item | selected <- False})) selectedId state.rootNode) (T.newNode state.rootNode newItem []) state.rootNode }

        RenameItem newName selectedId -> 
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | title <- newName, renaming <- False })) selectedId state.rootNode }

        UpdateItem newContent selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | content <- newContent })) selectedId state.rootNode }

        RenamingItem selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | renaming <- True })) selectedId state.rootNode }

        DeleteItem selectedId ->
            { state | rootNode <- T.removeNodeById selectedId state.rootNode }

        MoveNode movement selectedId ->
            { state | rootNode <- T.moveNodeById movement selectedId state.rootNode }

        ToggleExpanded selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | expanded <- not item.expanded })) selectedId state.rootNode }

        _ -> state

---------- VIEW ----------

view : State -> (Int, Int) -> Html
view state (w, h) = let
    w' = toFloat w
    h' = toFloat h
    in div [style [("height", "100%"),("border", "1px solid black")]] [
        div [style [("height", toString (h' * 0.06) ++ "px"), ("background-color", "grey")]] [text "top bar"],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.22) ++ "px"), ("height", toString (h' * 0.88) ++ "px"), ("border", "1px solid black")]] [treeToHtmlTree state.rootNode],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.01) ++ "px"), ("height", toString (h' * 0.88) ++ "px")]] [text "r"],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.77) ++ "px"), ("height", toString (h' * 0.88 - 5) ++ "px"), ("position", "absolute"), ("right", "1"), ("border", "1 px solid black")]]
            [textarea [style [("display", "inline-block"), ("float","right"), ("width", "100%"), ("height", "100%"), ("border", "none"), ("resize", "none")], placeholder "editor"] []],
        footer [style [("height", toString (h' * 0.06) ++ "px"), ("background-color", "grey")]] [text "footer"]
    ]

treeToHtmlTree : Tree Item -> Html
treeToHtmlTree (T.Node item children id) = ul [] [li [] <| [text item.title] ++ (List.map treeToHtmlTree children)]

---------- INPUTS ----------

main : Signal Html
main = Signal.map2 view state Window.dimensions

state : Signal State
state = Signal.foldp update emptyModel (Signal.constant NoOp)