module Model where

import Dialog exposing (Dialog)
import Tree as T
import Tree exposing (Tree)
import Context exposing (..)

import Json.Decode as Decoder
import Json.Decode exposing ((:=))
import Json.Encode as Encoder

---- MODELS ----

type alias State dModel dAct = {
    ui : UIState dModel dAct,
    rootNode : Tree Item,
    projectTitle : String,
    selectedId : Int
}

type alias UIState dModel dAct = {
    confirmationDialog : Maybe (Dialog dModel dAct),
    context : Context,
    lastContext : Context,
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

emptyModel : State dModel dAct
emptyModel = {
    rootNode = T.newNode T.Empty newItem [], 
    projectTitle = "",
    selectedId = 0,
    ui = emptyUIState
    }

emptyUIState : UIState dModel dAct
emptyUIState = {
    confirmationDialog = Nothing,
    context = Default,
    lastContext = Default,
    lastSelectedId = -1
    }

---- STATE JSON ENCODING / DECODING ----

encodeState : State dModel dAct -> String
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

decodeState : String -> Result String (State dModel dAct)
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