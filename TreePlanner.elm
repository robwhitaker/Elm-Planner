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
    ui : UIState,
    rootNode : Tree Item,
    projectTitle : String,
    selectedId : Int
}

type alias UIState = {
    confirmationDialog : Maybe Dialog,
    keyboardControlsEnabled : Bool,
    renamingCurrentNode : Bool
}

type alias Item = {
    title : String,
    content : String,
    expanded : Bool
}

type alias Dialog = {
    query   : String,
    confirm : Action,
    cancel  : Action
}

type alias UIEvent = {
    action : Action,
    isKeyboardGenerated : Bool,
    setKeyboardControlsEnabled : Maybe Bool
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
    projectTitle = "",
    selectedId = 0,
    ui = emptyUIState
    }

emptyDialog : Dialog
emptyDialog = {
    query = "",
    confirm = NoOp,
    cancel = Cancel
    }

emptyUIEvent : UIEvent
emptyUIEvent = {
    action = NoOp,
    isKeyboardGenerated = False,
    setKeyboardControlsEnabled = Nothing
    }

emptyUIState : UIState
emptyUIState = {
    confirmationDialog = Nothing,
    keyboardControlsEnabled = True,
    renamingCurrentNode = False
    }

---------- UPDATE ----------

type Action 
    = NewItem
    | Confirm Dialog
    | Cancel
    | NewProject
    | UpdateItemTitle String
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

update : UIEvent -> State -> State
update event state' = let
        ui' = state'.ui
        state = { state' | ui <- 
                    { ui' |
                        keyboardControlsEnabled <- Maybe.withDefault state'.ui.keyboardControlsEnabled event.setKeyboardControlsEnabled
                    }
                }
        ui = state.ui
        action = if not ui.keyboardControlsEnabled && event.isKeyboardGenerated then NoOp else event.action
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

        UpdateItemTitle newTitle -> { state | rootNode <- T.mapToNodeById (\item -> { item | title <- newTitle }) state.selectedId state.rootNode }

        Confirm dialog -> { state | ui <- { ui | confirmationDialog <- Just dialog, keyboardControlsEnabled <- False } }

        Cancel -> { state | ui <- { ui | confirmationDialog <- Nothing, keyboardControlsEnabled <- True } }

        SelectItem selectedId -> { state | selectedId <- selectedId }

        RenameProject newName -> { state | projectTitle <- newName }

        MoveSelection dir -> case dir of
            Up -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] 0) <| List.head <| List.reverse <| Utils.takeWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }
            Down -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] state.selectedId) <| List.head <| List.drop 1 <| Utils.dropWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }

        RenameItem newName -> 
            { state | rootNode <- T.mapToNodeById (\item -> { item | title <- newName }) state.selectedId state.rootNode, 
                      ui       <- { ui | renamingCurrentNode <- False }
            }

        UpdateItem newContent ->
            { state | rootNode <- T.mapToNodeById (\item -> { item | content <- newContent }) state.selectedId state.rootNode }

        RenamingItem selectedId -> case selectedId of
            Just sId -> { state | selectedId <- sId, ui <- { ui | renamingCurrentNode <- True } }
            Nothing  -> { state | ui <- { ui | renamingCurrentNode <- True } }

        DeleteItem -> let
                previousNode = Maybe.withDefault (T.dummyNode newItem) <| List.head <| List.reverse 
                               <| Utils.takeWhile ((/=) (T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode)) (T.flatten selectableNodes)
                prevId = if T.id previousNode < 0 then 0 else T.id previousNode
            in { state | rootNode <- T.removeNodeById state.selectedId state.rootNode, 
                         selectedId <- prevId, 
                         ui <- { ui | confirmationDialog <- Nothing }
               }

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

view : State -> (Int, Int) -> Html
view state (w, h) = let
        dialog = Maybe.withDefault emptyDialog state.ui.confirmationDialog
    in div [] [
        div [
            classList [("modal-background", True), ("no-display", dialog.query == "")],
            onClick uiEvent.address { emptyUIEvent | action <- dialog.cancel, setKeyboardControlsEnabled <- Just True }
        ] [],
        div [
            classList [("confirm-dialog", True), ("no-display", dialog.query == "")],
            style [("top", toString (h//2 - 167) ++ "px"),("left", toString (w//2 - 257) ++ "px")]
        ] [
            h2 [] [text dialog.query],
            div [] [
                button [
                    class "confirm-button",
                    onClick uiEvent.address { emptyUIEvent | action <- dialog.confirm, setKeyboardControlsEnabled <- Just True }
                ] [text "Confirm"],
                button [
                    class "cancel-button",
                    onClick uiEvent.address { emptyUIEvent | action <- dialog.cancel, setKeyboardControlsEnabled <- Just True }
                ] [text "Cancel"]
            ]
        ],
        div [class "title-bar"] [
            input [
                type' "text", 
                on "input" targetValue (Signal.message uiEvent.address << (\act ->
                    { emptyUIEvent | action <- act, setKeyboardControlsEnabled <- Just False }
                ) << RenameProject),
                onFocus uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just False },
                onBlur uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just True },
                value state.projectTitle,
                placeholder "Untitled Project"
            ] []
        ],
        div [class "options-bar"] [
            img [onClick uiEvent.address { emptyUIEvent | action <- Confirm 
                { emptyDialog | query <- "Are you sure you want to create a new project? Any unsaved progress will be lost.",
                                confirm <- NewProject
                }
                }, src "new.png", class "icon"] [],
            img [onClick saveFile.address (), src "save.png", class "icon"] [],
            label [for "loadButton"] [
                input  [type' "file", id "loadButton"] [text "Load"],
                img [class "icon", src "load.png"] []
            ] 
        ],
        div [
            class "main-container",
            style [("height", toString (h-90) ++ "px")]
        ] [
            div [
                class "tree-pane",
                onClick uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just True }
            ] [lazy2 treeToHtmlTree state state.rootNode],
            div [
                class "text-area-container"
            ] [
                textarea [
                   placeholder "...",
                   value (T.value (T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode)).content,
                   on "input" targetValue (Signal.message uiEvent.address << (\act -> 
                        { emptyUIEvent | action <- act, setKeyboardControlsEnabled <- Just False }
                    ) << UpdateItem),
                    onFocus uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just False },
                    onBlur uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just True }
                ] []
            ]
        ]
    ]

treeToHtmlTree : State -> Tree Item -> Html
treeToHtmlTree state (T.Node item children id') = let
        liContent = if state.ui.renamingCurrentNode && id' == state.selectedId
                    then 
                        input [ 
                            type' "text",
                            value item.title,
                            id ("node-" ++ toString id'), 
                            onEnter targetValue (Signal.message uiEvent.address << (\act -> 
                                { emptyUIEvent | action <- act }
                            ) << RenameItem),
                            on "blur" targetValue (Signal.message uiEvent.address << (\act ->
                                { emptyUIEvent | action <- act, setKeyboardControlsEnabled <- Just True }
                            ) << RenameItem),
                            on "input" targetValue (Signal.message uiEvent.address << (\act -> { emptyUIEvent | action <- act }) << UpdateItemTitle),
                            onFocus uiEvent.address { emptyUIEvent | setKeyboardControlsEnabled <- Just False }
                        ] []
                    else 
                        div [
                            classList [
                                ("item-title", True),
                                ("selected-focused", id' == state.selectedId && state.ui.keyboardControlsEnabled), 
                                ("selected-unfocused", id' == state.selectedId && (not state.ui.keyboardControlsEnabled))
                            ],
                            onClick uiEvent.address { emptyUIEvent | action <- SelectItem id' }, 
                            onDoubleClick uiEvent.address { emptyUIEvent | action <- RenamingItem (Just id') }
                        ] [text item.title]
        in ul [classList [("root-node", id' == 0)]] [
            li [
                classList [("hidden", not item.expanded && children /= [])]
            ] <| 
                div [class "arrow-container"] [
                    img [
                        class "expand-arrow-icon",
                        src (if item.expanded then "arrow-expanded.png" else "arrow-collapsed.png"), 
                        onClick uiEvent.address { emptyUIEvent | action <- ToggleExpanded (Just id') }
                    ] []
                ] 
                :: liContent 
                :: (if item.expanded then List.map (lazy2 treeToHtmlTree state) children else [])]

onEnter : Decoder.Decoder a -> (a -> Signal.Message) -> Attribute
onEnter decoder f = on "keydown"
                    (Decoder.object2 (\code val -> if code == 13 then Ok val else Err "") keyCode decoder)
                    (\result -> case result of
                        Ok value  -> f value
                        Err _ -> Signal.message errBox.address ()
                    )

---------- INPUTS ----------

main : Signal Html
main = Signal.map2 view state Window.dimensions

initialModel : State
initialModel = decodeState getStorage |> \result -> case result of
    Ok model -> let
        ui = model.ui
        in { model | ui <- { ui | renamingCurrentNode <- False } }
    _        -> emptyModel

state : Signal State
state = Signal.foldp update initialModel uiInput

keyboardInput : Signal UIEvent
keyboardInput = Signal.dropRepeats Keyboard.keysDown
    |> Signal.map (\keypresses ->
        case (Set.toList keypresses) of
            [17, 37] -> { emptyUIEvent | action <- MoveNode T.Lift, isKeyboardGenerated <- True }
            [17, 38] -> { emptyUIEvent | action <- MoveNode T.ShiftUp, isKeyboardGenerated <- True }
            [17, 39] -> { emptyUIEvent | action <- MoveNode T.Lower, isKeyboardGenerated <- True }
            [17, 40] -> { emptyUIEvent | action <- MoveNode T.ShiftDown, isKeyboardGenerated <- True }
            [13, 17] -> { emptyUIEvent | action <- NewItem, isKeyboardGenerated <- True }
            [17, 46] -> { emptyUIEvent | action <- Confirm 
                            { emptyDialog |
                                query   <- "Are you sure you want to delete this item and all sub-items?",
                                confirm <- DeleteItem
                            },
                            isKeyboardGenerated <- True
                        }
            [32]     -> { emptyUIEvent | action <- ToggleExpanded Nothing, isKeyboardGenerated <- True }
            [13]     -> { emptyUIEvent | action <- RenamingItem Nothing, isKeyboardGenerated <- True }
            [38]     -> { emptyUIEvent | action <- MoveSelection Up, isKeyboardGenerated <- True }
            [40]     -> { emptyUIEvent | action <- MoveSelection Down , isKeyboardGenerated <- True}
            _        -> emptyUIEvent
    )
    |> Signal.filter ((/=) emptyUIEvent) emptyUIEvent

load : Signal UIEvent
load = (decodeState >> \result -> case result of
    Ok model -> { emptyUIEvent | action <- Confirm
                    { emptyDialog | 
                        query   <- "Are you sure you want to open this file? Any unsaved progress will be lost.",
                        confirm <- let
                            ui = model.ui
                            in LoadProject { model | ui <- { ui | renamingCurrentNode <- False } } 
                    }
                }
    Err _    -> emptyUIEvent
    ) <~ fileUpload

uiInput : Signal UIEvent
uiInput = Signal.mergeMany [uiEvent.signal, keyboardInput, load]

---------- MAILBOXES ----------

uiEvent : Signal.Mailbox UIEvent
uiEvent = Signal.mailbox emptyUIEvent

saveFile : Signal.Mailbox ()
saveFile = Signal.mailbox ()

errBox : Signal.Mailbox ()
errBox = Signal.mailbox ()

---------- PORTS ----------

port focus : Signal String
port focus = Signal.filter ((/=) "") "" 
          <| Signal.dropRepeats 
          <| Signal.map (\s -> if s.ui.renamingCurrentNode then "#node-" ++ toString s.selectedId else "") state

port save : Signal (String, String)
port save = let
    fileName = String.words >> String.join "_"
    in
        Signal.sampleOn saveFile.signal
        <| Signal.map (\s -> (,)
                             (if s.projectTitle == "" then "untitled" else fileName s.projectTitle) 
                             (encodeState s)
                      ) state
        
port getStorage : String

port setStorage : Signal String
port setStorage = Signal.map encodeState state 

port fileUpload : Signal String

port log : Signal String
port log = Signal.constant ""

---------- HELPERS ----------

encodeState : State -> String
encodeState s = let
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

    encodeState' s = 
        Encoder.object [
            ("rootNode", encodeTree s.rootNode),
            ("projectTitle", Encoder.string s.projectTitle),
            ("selectedId", Encoder.int s.selectedId)
        ]
    in Encoder.encode 0 <| encodeState' s

decodeState : String -> Result String State
decodeState s = let 
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

    stateDecoder = Decoder.object3 (State emptyUIState) 
                                         ("rootNode"            := treeDecoder)
                                         ("projectTitle"        := Decoder.string) 
                                         ("selectedId"          := Decoder.int)
    in (Decoder.decodeString stateDecoder s) 
