module TreePlanner where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Json.Decode as Decoder
import Json.Decode exposing ((:=))
import Json.Encode as Encoder
import Set
import String
import Utils

import Signal
import Signal exposing (Signal, (<~))
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
    selectedId : Int,
    renamingCurrentNode : Bool
}

type alias Item = {
    title : String,
    content : String,
    expanded : Bool
}

newItem : Item 
newItem = {
    title = "new",
    content = "",
    expanded = True
    }

emptyModel : State
emptyModel = {
    rootNode = T.newNode T.Empty newItem [], 
    projectTitle = "untitled",
    sidebarSize = 0.2,
    selectedId = 0,
    renamingCurrentNode = False
    }

---------- UPDATE ----------

type Action 
    = NewItem
    | NewProject
    | LoadProject State
    | SelectItem Int
    | MoveSelection SelectionMovement
    | RenameItem String
    | UpdateItem String
    | RenamingItem (Maybe Int)
    | RenameProject String
    | DeleteItem
    | MoveNode NodeMovement
    | ToggleExpanded (Maybe Int)
    | NoOp

type SelectionMovement = Up | Down
update : Action -> State -> State
update action state = let
            selectableNodes = T.mapN (\(T.Node value children id) -> if not value.expanded then (T.Node value [] id) else (T.Node value children id)) state.rootNode
        in case action of
        NewItem -> let
            parent = T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode
            child = T.newNode state.rootNode newItem []
            newTree = T.addChildTo parent child state.rootNode
                      |> T.moveNode T.Lift child
            in { state | rootNode <- newTree, selectedId <- T.id child }

        NewProject -> emptyModel

        LoadProject model -> model

        SelectItem selectedId -> { state | selectedId <- selectedId }

        RenameProject newName -> { state | projectTitle <- newName }

        MoveSelection dir -> case dir of
            Up -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] 0) <| List.head <| List.reverse <| Utils.takeWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }
            Down -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] state.selectedId) <| List.head <| List.drop 1 <| Utils.dropWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }

        RenameItem newName -> 
            { state | rootNode <- T.mapToNodeById (\item -> { item | title <- newName }) state.selectedId state.rootNode, renamingCurrentNode <- False }

        UpdateItem newContent ->
            { state | rootNode <- T.mapToNodeById (\item -> { item | content <- newContent }) state.selectedId state.rootNode }

        RenamingItem selectedId -> case selectedId of
            Just sId -> { state | selectedId <- sId, renamingCurrentNode <- True }
            Nothing  -> { state | renamingCurrentNode <- True }

        DeleteItem -> let
                previousNode = Maybe.withDefault (T.dummyNode newItem) <| List.head <| List.reverse 
                               <| Utils.takeWhile ((/=) (T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode)) (T.flatten selectableNodes)
                prevId = if T.id previousNode < 0 then 0 else T.id previousNode
            in { state | rootNode <- T.removeNodeById state.selectedId state.rootNode, selectedId <- prevId }

        MoveNode movement -> case movement of
            T.Lower -> let 
                newState = { state | rootNode <- T.moveNodeById movement state.selectedId state.rootNode }
                parent = T.parent (T.nodeByIdWithDefault state.rootNode state.selectedId newState.rootNode) newState.rootNode |> Maybe.withDefault state.rootNode
                in { newState | rootNode <- T.mapToNodeById (\item -> { item | expanded <- True }) (T.id parent) newState.rootNode }

            _       -> { state | rootNode <- T.moveNodeById movement state.selectedId state.rootNode }

        ToggleExpanded selectedId -> case selectedId of
            Just sId -> let
                toggledNode = T.nodeByIdWithDefault (T.dummyNode newItem) sId state.rootNode
                in case T.nodeById state.selectedId toggledNode of
                    Just _ -> 
                        { state | 
                            rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) sId state.rootNode,
                            selectedId <- sId 
                        }
                    Nothing -> { state | rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) sId state.rootNode }
            Nothing  -> 
                { state | rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) state.selectedId state.rootNode }

        _ -> state

---------- VIEW ----------

view : State -> (Int, Int) -> Bool -> Html
view state (w, h) keysEnabled = let
    w' = toFloat w
    h' = toFloat h
    in div [style [("height", "100%"),("border", "1px solid black")]] [
        div [style [("height", toString (h' * 0.06) ++ "px"), ("background-color", "grey")]] [
            button [onClick saveFile.address ()] [text "Save"],
            input  [type' "file", id "loadButton"] [text "Load"],
            button [onClick uiInput.address NewProject] [text "New Project"],
            input [
                type' "text", 
                on "input" targetValue (Signal.message uiInput.address << RenameProject),
                value state.projectTitle,
                onFocus keyboardControlsEnabled.address False,
                onBlur keyboardControlsEnabled.address True
            ] []
        ],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.22) ++ "px"), ("height", toString (h' * 0.88) ++ "px"), ("border", "1px solid black")],
            onClick keyboardControlsEnabled.address True
        ] [lazy3 treeToHtmlTree state keysEnabled state.rootNode],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.01) ++ "px")]] [],
        div [style [("display", "inline-block"), ("width", toString (w' * 0.76) ++ "px"), ("height", toString (h' * 0.88 - 5) ++ "px"), ("position", "absolute"), ("right", "1"), ("border", "1 px solid black")],
            onClick keyboardControlsEnabled.address False,
            onBlur keyboardControlsEnabled.address True
        ]
            [textarea [style [("display", "inline-block"), ("float","right"), ("width", "100%"), ("height", "100%"), ("border", "none"), ("resize", "none")], 
                       placeholder "editor",
                       value (T.value (T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode)).content,
                       on "change" targetValue (Signal.message uiInput.address << UpdateItem),
                       onBlur keyboardControlsEnabled.address True,
                       onFocus keyboardControlsEnabled.address False
                      ] []]
    ]

treeToHtmlTree : State -> Bool -> Tree Item -> Html
treeToHtmlTree state keysEnabled (T.Node item children id') = let
        liContent = if state.renamingCurrentNode && id' == state.selectedId
                    then 
                        input [   
                            value item.title,
                            id ("node-" ++ toString id'), 
                            onEnter targetValue (Signal.message uiInput.address << RenameItem),
                            on "blur" targetValue (Signal.message uiInput.address << RenameItem),
                            onFocus keyboardControlsEnabled.address False
                        ] []
                    else 
                        div [
                            classList [("selected-focused", id' == state.selectedId && keysEnabled), ("selected-unfocused", id' == state.selectedId && (not keysEnabled))],
                            onClick uiInput.address (SelectItem id'), 
                            onDoubleClick uiInput.address (RenamingItem (Just id')),
                            style [("display", "inline-block")]
                        ] [text item.title]
        in ul [] [
            li [classList [("hidden", not item.expanded)]] <| 
                img [src (if item.expanded then "arrow-expanded.png" else "arrow-collapsed.png"), 
                    width 25, 
                    height 25,
                    onClick uiInput.address (ToggleExpanded (Just id'))
                    ] [] 
                :: liContent 
                :: (if item.expanded then List.map (lazy3 treeToHtmlTree state keysEnabled) children else [])]

onEnter : Decoder.Decoder a -> (a -> Signal.Message) -> Attribute
onEnter decoder f = on "keydown"
                    (Decoder.object2 (\code val -> if code == 13 then Ok val else Err "") keyCode decoder)
                    (\result -> case result of
                        Ok value  -> f value
                        Err _ -> Signal.message errBox.address ()
                    )

---------- INPUTS ----------

main : Signal Html
main = Signal.map3 view state Window.dimensions keyboardControlsEnabled'

state : Signal State
state = Signal.foldp update emptyModel <| Signal.mergeMany [uiInput.signal, keyboardInput, load]

keyboardInput : Signal Action
keyboardInput = Signal.map2 (,) keyboardControlsEnabled' Keyboard.keysDown
    |> Signal.dropRepeats
    |> Signal.map (\(enabled, keypresses) -> if not enabled then NoOp else
        case (Set.toList keypresses) of
            [17, 37] -> MoveNode T.Lift
            [17, 38] -> MoveNode T.ShiftUp
            [17, 39] -> MoveNode T.Lower
            [17, 40] -> MoveNode T.ShiftDown
            [13, 17] -> NewItem
            [17, 46] -> DeleteItem
            [32]     -> ToggleExpanded Nothing
            [13]     -> RenamingItem Nothing
            [38]     -> MoveSelection Up
            [40]     -> MoveSelection Down
            _        -> NoOp
    )
    |> Signal.filter ((/=) NoOp) NoOp


keyboardControlsEnabled' : Signal Bool
keyboardControlsEnabled' = Signal.merge keyboardControlsEnabled.signal 
    <| Signal.map (\action ->
            case action of
                RenamingItem _ -> False
                RenameProject _ -> False
                _ -> True
        ) uiInput.signal

load : Signal Action
load = let 
    itemDecoder  = Decoder.object3 Item ("title"    := Decoder.string) 
                                        ("content"  := Decoder.string) 
                                        ("expanded" := Decoder.bool)

    lazy : (() -> Decoder.Decoder a) -> Decoder.Decoder a
    lazy thunk =
      Decoder.customDecoder Decoder.value
          (\js -> Decoder.decodeValue (thunk ()) js)

    treeDecoder  = Decoder.object3 T.Node ("value"    := itemDecoder) 
                                          ("children" := Decoder.list (lazy (\_ -> treeDecoder))) 
                                          ("id"       := Decoder.int)

    stateDecoder = Decoder.object5 State ("rootNode"            := treeDecoder)
                                         ("projectTitle"        := Decoder.string) 
                                         ("sidebarSize"         := Decoder.float)
                                         ("selectedId"          := Decoder.int)
                                         ("renamingCurrentNode" := Decoder.bool)
    in (Decoder.decodeString stateDecoder >> \result ->
            case result of
                Ok  model -> LoadProject model
                Err _     -> NewProject
        ) <~ fileUpload

---------- MAILBOXES ----------

uiInput : Signal.Mailbox Action
uiInput = Signal.mailbox NoOp

saveFile : Signal.Mailbox ()
saveFile = Signal.mailbox ()

keyboardControlsEnabled : Signal.Mailbox Bool
keyboardControlsEnabled = Signal.mailbox True

loadButton : Signal.Mailbox String
loadButton = Signal.mailbox ""

errBox : Signal.Mailbox ()
errBox = Signal.mailbox ()

---------- PORTS ----------

port focus : Signal String
port focus = Signal.filter ((/=) "") "" 
          <| Signal.dropRepeats 
          <| Signal.map (\s -> if s.renamingCurrentNode then "#node-" ++ toString s.selectedId else "") state

port save : Signal (String, String)
port save = let
    encodeItem item = Encoder.object [
            ("title", Encoder.string item.title),
            ("content", Encoder.string item.content),
            ("expanded", Encoder.bool item.expanded)
        ]
    
    encodeTree (T.Node value children id) = 
        Encoder.object [
            ("value", encodeItem value),
            ("children", List.map encodeTree children |> Encoder.list),
            ("id", Encoder.int id)
        ]

    encodeState s = 
        Encoder.object [
            ("rootNode", encodeTree s.rootNode),
            ("projectTitle", Encoder.string s.projectTitle),
            ("sidebarSize", Encoder.float s.sidebarSize),
            ("selectedId", Encoder.int s.selectedId),
            ("renamingCurrentNode", Encoder.bool s.renamingCurrentNode)
        ]

    fileName = String.words >> String.join "_"
    in
        Signal.sampleOn saveFile.signal
        <| Signal.map (\s -> (fileName s.projectTitle, (Encoder.encode 0 << encodeState) s)) state 

port fileUpload : Signal String

port log : Signal String
port log = Signal.constant ""
