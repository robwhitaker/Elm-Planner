module Planner.Main where

import Planner.Model exposing (..)
import Planner.Event exposing (..)
import Planner.UI.Context exposing (..)
import Planner.Update exposing (update)
import Planner.View as View
import Planner.Data.Tree as T
import Planner.Component.Text as Text

import Set
import String
import Html exposing (Html)
import Signal
import Signal exposing (Signal, (<~), (~))
import Window
import Keyboard

---- MAIN ----

main : Signal Html
main = View.render uiEvent.address <~ state ~ Window.dimensions

---- INPUTS ----

initialModel : State
initialModel = decodeState getStorage |> \result -> case result of
    Ok model -> let
        ui = model.ui
        in { model | ui <- { ui | context <- Default } }
    _        -> emptyModel

state : Signal State
state = Signal.foldp update initialModel inputEvent

keyboardInput : Signal (List Int)
keyboardInput = Set.toList <~ Signal.dropRepeats Keyboard.keysDown 

load : Signal Event
load = (\json -> { emptyEvent | action <- LoadProject json }) <~ fileUpload

inputEvent : Signal Input
inputEvent = Signal.mergeMany [UIEvent <~ uiEvent.signal, KeyboardEvent <~ keyboardInput, UIEvent <~ load]

---- MAILBOXES ----

uiEvent : Signal.Mailbox Event
uiEvent = Signal.mailbox emptyEvent

---- PORTS ----

port focus : Signal (String, Maybe String)
port focus = Signal.filter ((/=) "" << fst) ("", Nothing) 
          <| Signal.dropRepeats 
          <| Signal.map (\s -> case s.ui.context of
                                RenamingNode -> ("#node-" ++ toString s.selectedId, Maybe.map (.title << T.value) <| T.nodeById s.selectedId s.rootNode)
                                TitleInput   -> ("#title-bar-input", Just s.projectTitle)
                                MainTextArea -> ("#textbox", Nothing)
                                _ -> ("default", Nothing)
                        ) state

port scroll : Signal String
port scroll = Signal.map (\s -> "#node-" ++ toString s.selectedId) state

port ticker : Signal Bool

port textBoxText : Signal String
port textBoxText = let
    isSelection = Signal.map (\s -> s.selectedId /= s.ui.lastSelectedId) state 
    in Signal.sampleOn (Signal.merge (Signal.filter identity False isSelection) ticker)
    <| Signal.map (\s -> Maybe.withDefault "" <| Maybe.map (.content << T.value) <| T.nodeById s.selectedId s.rootNode) state

port save : Signal (String, String)
port save = let
    fileName = String.words >> String.join "_"
    in
        Signal.sampleOn 
            (Signal.merge 
                (Signal.filterMap (\event -> if event.action == SaveProject then Just () else Nothing) () uiEvent.signal)
                (Signal.filterMap (\keys -> if keys == [17, 83] then Just () else Nothing) () keyboardInput)
            )
                <| Signal.map (\s -> (,)
                             (if s.projectTitle == "" then Text.untitledSaveTitle else fileName s.projectTitle) 
                             (encodeState s)
                      ) state
        
port getStorage : String

port setStorage : Signal String
port setStorage = Signal.map encodeState state 

port fileUpload : Signal String

port log : Signal String
port log = Signal.constant ""
