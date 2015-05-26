module Dialog where

import Event exposing (..)
import Context exposing (..)

import Maybe exposing (andThen)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal

---- MODEL ----

type alias Dialog m = {
    model            : m,
    update           : DialogAction a -> m -> m,
    view             : (Int,Int) -> Signal.Address -> m -> Html,
    keyboardInputMap : List Int -> DialogAction a
}

type DialogAction a = Show | Hide | Custom a

---- UPDATE ----

update : DialogAction a -> Maybe (Dialog m a) -> Maybe (Dialog m a)
update action dialog = 
    dialog `andThen` \dial ->
        case action of
            Show -> Just dial
            Hide -> Nothing
            Custom act -> Just <| { dial | model <- dial.update act dial.model }

---- DIALOG DEFINITIONS ----

-- Confirm Dialog

type alias ConfirmDialogModel = {
    query           : String,
    confirm         : Action,
    cancel          : Action,
    selectedOption  : Int
}

type ConfirmDialogAction = ChangeSelection Int

emptyConfirmDialogModel : ConfirmDialogModel
emptyConfirmDialogModel = {
    query = "",
    confirm = NoOp,
    cancel = Cancel, 
    selectedOption = 0
    }

confirmDialog : ConfirmDialogModel -> Dialog ConfirmDialogModel ConfirmDialogAction
confirmDialog model = {
    model = model,
    
    update action dialModel = 
        case action of
            ChangeSelection selection -> { dialModel | selectedOption <- selection },
    
    view (w,h) address dialModel = 
        div [] [
            div [
                class "modal-background"
                onClick address { emptyEvent | action <- dialog.cancel, setContext <- Just Default }
            ] [],
            div [
                class "confirm-dialog"
                style [("top", toString (h//2 - 167) ++ "px"),("left", toString (w//2 - 257) ++ "px")]
            ] [
                h2 [] [text dialModel.query],
                div [] [
                    button [
                        classList [("confirm-button", dialModel.selectedOption == 0), ("cancel-button", dialModel.selectedOption /= 0)]
                        onClick address { emptyEvent | action <- dialModel.confirm, setContext <- Just Default }
                    ] [text "Confirm"],
                    button [
                        classList [("confirm-button", dialModel.selectedOption /= 0), ("cancel-button", dialModel.selectedOption == 0)]
                        onClick address { emptyEvent | action <- dialModel.cancel, setContext <- Just Default }
                    ] [text "Cancel"]
                ]
            ]
        ],
        
        keyboardInputMap keypresses = 
            [13]     -> { emptyEvent | action <- confirmAction, setContext <- Just Default }
            [27]     -> { emptyEvent | action <- dialog.cancel, setContext <- Just Default }
            [37]     -> { emptyEvent | action <- ChangeConfirmSelection 0 }
            [39]     -> { emptyEvent | action <- ChangeConfirmSelection 1 }
            _        -> emptyEvent
    } 