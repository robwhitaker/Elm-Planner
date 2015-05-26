module Dialogs where

import Dialog exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

---- CONFIRM DIALOG ----

type alias ConfirmDialogModel = {
    query           : String,
    confirm         : String,
    cancel          : String,
    selectedOption  : Int
}

type ConfirmDialogAction = ChangeSelection Int

emptyConfirmDialogModel : ConfirmDialogModel
emptyConfirmDialogModel = {
    query = "",
    confirm = "NoOp",
    cancel = "Cancel", 
    selectedOption = 0
    }

confirmDialog : ConfirmDialogModel -> Dialog ConfirmDialogModel ConfirmDialogAction
confirmDialog model = {
    model = model,
    update action dialModel = 
        case action of
            ChangeSelection selection -> { dialModel | selectedOption <- selection },
    view dialModel = 
        div [] [
            div [
                class "modal-background"
                --onClick uiEvent.address { emptyEvent | action <- dialog.cancel, setContext <- Just Default }
            ] [],
            div [
                class "confirm-dialog"
                --style [("top", toString (h//2 - 167) ++ "px"),("left", toString (w//2 - 257) ++ "px")]
            ] [
                h2 [] [text dialModel.query],
                div [] [
                    button [
                        classList [("confirm-button", dialModel.selectedOption == 0), ("cancel-button", dialModel.selectedOption /= 0)]
                        --onClick uiEvent.address { emptyEvent | action <- dialModel.confirm, setContext <- Just Default }
                    ] [text "Confirm"],
                    button [
                        classList [("confirm-button", dialModel.selectedOption /= 0), ("cancel-button", dialModel.selectedOption == 0)]
                        --onClick uiEvent.address { emptyEvent | action <- dialModel.cancel, setContext <- Just Default }
                    ] [text "Cancel"]
                ]
            ]
        ],
        keyboardInputMap keypresses = ChangeSelection 1
    } 