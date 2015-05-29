module Planner.UI.Dialog where

import Maybe exposing (andThen)
import Html exposing (Html)

---- MODEL ----

type alias Dialog event = {
    model  : Model event,
    update : CustomAction -> Model event -> Model event,
    view   : (Int,Int) -> Signal.Address event -> Model event -> Html,
    keyboardInputMap : List Int -> Model event -> event
}

type Model event 
    = ConfirmDialog { query : String, confirm : event, cancel : event, selectedOption : Int }
    | KeyboardShortcutDialog { keyMap : List (String, String) }

type Action event
    = Show (Maybe (Dialog event))
    | Hide
    | HideWith event
    | Custom CustomAction

type CustomAction 
    = ChangeSelection Int 

---- UPDATE ----

update : Action event -> Maybe (Dialog event) -> Maybe (Dialog event)
update action dialog = 
    case action of
        Show newDialog -> newDialog
        _ -> dialog `andThen` \dial ->
            case action of
                Hide       -> Nothing
                HideWith _ -> Nothing
                Custom act -> Just <| { dial | model <- dial.update act dial.model }
                _ -> Nothing

