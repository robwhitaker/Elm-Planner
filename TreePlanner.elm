module TreePlanner where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Json.Decode as Json
import Set
import Utils

import Signal
import Signal exposing (Signal)
import Window
import Keyboard

import Tree as T
import Tree exposing (Tree, NodeMovement)

import Debug

---------- MODEL ----------

-- Application state
type alias State = {
    rootNode : Tree Item,
    projectTitle : String,
    sidebarSize : Float,
    selectedId : Int
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
    selected = False,
    renaming = False
    }

emptyModel : State
emptyModel = {
    rootNode = T.Node { newItem | selected <- True } [T.Node newItem [] 1, T.Node newItem [T.Node newItem [] 3] 2] 0, 
    projectTitle = "Untitled",
    sidebarSize = 0.2,
    selectedId = 0
    }

---------- UPDATE ----------

type Action 
    = NewItem
    | SelectItem Int
    | MoveSelection SelectionMovement
    | RenameItem String
    | UpdateItem String
    | RenamingItem Int
    | DeleteItem
    | MoveNode NodeMovement
    | ToggleExpanded Int
    | NoOp

type SelectionMovement = Up | Down
update : Action -> State -> State
update action state = case action of
        NewItem -> let
            getNodeFromId = (Maybe.withDefault (T.dummyNode newItem) << flip T.nodeById state.rootNode)
            parent = getNodeFromId state.selectedId
            child = T.newNode state.rootNode newItem []
            newTree = T.addChildTo parent child state.rootNode
                      |> T.moveNode T.Lift child
            in { state | rootNode <- newTree, selectedId <- T.id child }

        SelectItem selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | selected <- True })) selectedId 
                               <| T.map (T.valueM (\item -> { item | selected <- False, renaming <- False })) state.rootNode, 
                      selectedId <- Debug.log "ehh" selectedId }

        MoveSelection dir -> case dir of
            Up -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] 0) <| List.head <| List.reverse <| Utils.takeWhile ((/=) state.selectedId << T.id) (T.flatten state.rootNode) }
            Down -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] state.selectedId) <| List.head <| List.drop 1 <| Utils.dropWhile ((/=) state.selectedId << T.id) (T.flatten state.rootNode) }

        RenameItem newName -> 
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | title <- newName, renaming <- False, selected <- True })) 
                                                   state.selectedId 
                                                   state.rootNode }

        UpdateItem newContent ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | content <- newContent })) state.selectedId state.rootNode }

        RenamingItem selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | renaming <- True, selected <- True })) selectedId 
                               <| T.map (T.valueM (\item -> { item | renaming <- False, selected <- False })) state.rootNode }

        DeleteItem ->
            { state | rootNode <- T.removeNodeById state.selectedId state.rootNode }

        MoveNode movement ->
            { state | rootNode <- T.moveNodeById movement state.selectedId state.rootNode }

        ToggleExpanded selectedId ->
            { state | rootNode <- T.mapToNodeById (T.valueM (\item -> { item | expanded <- not item.expanded })) selectedId state.rootNode }

        _ -> state

---------- VIEW ----------

view : State -> (Int, Int) -> Html
view state (w, h) = let
    w' = toFloat w
    h' = toFloat h
    getNodeFromId = (Maybe.withDefault (T.dummyNode newItem) << flip T.nodeById state.rootNode)
    in div [style [("height", "100%"),("border", "1px solid black")]] [
        div [style [("height", toString (h' * 0.06) ++ "px"), ("background-color", "grey")]] [text "top bar"],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.22) ++ "px"), ("height", toString (h' * 0.88) ++ "px"), ("border", "1px solid black")],
            onClick keyboardControls.address True
        ] [lazy2 treeToHtmlTree state state.rootNode],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.01) ++ "px")]] [],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.76) ++ "px"), ("height", toString (h' * 0.88 - 5) ++ "px"), ("position", "absolute"), ("right", "1"), ("border", "1 px solid black")],
            onClick keyboardControls.address False,
            onBlur keyboardControls.address True
        ]
            [textarea [style [("display", "inline-block"), ("float","right"), ("width", "100%"), ("height", "100%"), ("border", "none"), ("resize", "none")], 
                       placeholder "editor",
                       value (T.value (getNodeFromId state.selectedId)).content,
                       on "change" targetValue (Signal.message uiInput.address << UpdateItem)
                      ] []]
    ]

treeToHtmlTree : State -> Tree Item -> Html
treeToHtmlTree state (T.Node item children id') = let
        liContent = if item.renaming 
                    then 
                        input [   
                            value item.title,
                            id ("node-" ++ toString id'), 
                            on "change" targetValue (Signal.message uiInput.address << RenameItem), 
                            onBlur keyboardControls.address True,
                            onFocus keyboardControls.address False,
                            onEnter targetValue (Signal.message uiInput.address << RenameItem)
                        ] []
                    else 
                        div [
                            classList [("selected", id' == state.selectedId)],
                            onClick uiInput.address (SelectItem id'), 
                            onDoubleClick uiInput.address (RenamingItem id')
                        ] [text item.title]
        in ul [] [
            li [] <| liContent :: (List.map (lazy2 treeToHtmlTree state) children)]

onEnter : Json.Decoder a -> (a -> Signal.Message) -> Attribute
onEnter decoder f = on "keydown"
                    (Json.object2 (\code val -> if code == 13 then Ok val else Err "") keyCode decoder)
                    (\(Ok value) -> f <| Debug.log "val" value)

---------- INPUTS ----------

main : Signal Html
main = Signal.map2 view state Window.dimensions

state : Signal State
state = Signal.foldp update emptyModel <| Signal.merge uiInput.signal keyboardInput

--TODO: add SampleOn so this won't interfere with renaming nodes
keyboardInput : Signal Action
keyboardInput = Signal.map2 (,) keyboardControlsEnabled Keyboard.keysDown
    |> Signal.dropRepeats
    |> Signal.map (\(enabled, keypresses) -> if not enabled then NoOp else
        case (Set.toList keypresses) of
            [17, 37] -> MoveNode T.Lift
            [17, 38] -> MoveNode T.ShiftUp
            [17, 39] -> MoveNode T.Lower
            [17, 40] -> MoveNode T.ShiftDown
            [13, 17] -> NewItem
            [17, 46] -> DeleteItem
            [38]     -> MoveSelection Up
            [40]     -> MoveSelection Down
            _        -> NoOp
    )
    |> Signal.filter ((/=) NoOp) NoOp


keyboardControlsEnabled : Signal Bool
keyboardControlsEnabled = Signal.foldp (always << identity) True keyboardControls.signal

---------- WIRING ----------

uiInput : Signal.Mailbox Action
uiInput = Signal.mailbox NoOp

keyboardControls : Signal.Mailbox Bool
keyboardControls = Signal.mailbox True

port focus : Signal String
port focus = let
    needsFocus action = case action of
        RenamingItem id -> True
        _               -> False

    toSelector (RenamingItem id) = "#node-" ++ toString id
    in uiInput.signal |> Signal.filter needsFocus (RenamingItem -1) |> Signal.map toSelector
