module Planner.Model where

import Planner.Event as Event
import Planner.Data.Tree as Tree
import Planner.UI.Context as Context
import Planner.UI.Dialog as Dialog

import Json.Decode as Decoder
import Json.Decode exposing ((:=))
import Json.Encode as Encoder

---- MODELS ----

type alias State = {
    ui : UIState,
    rootNode : Tree.Tree Item,
    projectTitle : String,
    selectedId : Int
}

type alias UIState = {
    dialog : Maybe (Dialog.Dialog Event.Event),
    context : Context.Context,
    lastContext : Context.Context,
    lastSelectedId : Int
}

type alias Item = {
    title : String,
    content : String,
    expanded : Bool
}

---- DEFAULT VALUES ----

newItem : Item 
newItem = {
    title = "new",
    content = "",
    expanded = True
    }

emptyModel : State
emptyModel = {
    rootNode = Tree.newNode Tree.Empty newItem [], 
    projectTitle = "",
    selectedId = 0,
    ui = emptyUIState
    }

emptyUIState : UIState
emptyUIState = {
    dialog = Nothing,
    context = Context.Default,
    lastContext = Context.Default,
    lastSelectedId = -1
    }

---- STATE JSON ENCODING / DECODING ----

encodeState : State -> String
encodeState s = let
    encodeItem item = Encoder.object [
            ("title", Encoder.string item.title),
            ("content", Encoder.string item.content),
            ("expanded", Encoder.bool item.expanded)
        ]
    
    encodeTree (Tree.Node value children id) = 
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

    treeDecoder  = Decoder.object3 Tree.Node ("value"    := itemDecoder) 
                                          ("children" := Decoder.list (lazy (\_ -> treeDecoder))) 
                                          ("id"       := Decoder.int)

    stateDecoder = Decoder.object3 (State emptyUIState) 
                                         ("rootNode"            := treeDecoder)
                                         ("projectTitle"        := Decoder.string) 
                                         ("selectedId"          := Decoder.int)
    in (Decoder.decodeString stateDecoder s) 
