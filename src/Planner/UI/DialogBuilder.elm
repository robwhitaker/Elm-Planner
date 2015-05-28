module Planner.UI.DialogBuilder where

import Planner.UI.Dialog as Dialog
import Planner.Event as Event
import Planner.Event exposing (emptyEvent)
import Planner.UI.Context as Context

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe

---- CONFIRM DIALOG ----

emptyConfirmDialogModel : Dialog.Model Event.Event
emptyConfirmDialogModel = Dialog.ConfirmDialog {
    query = "",
    confirm = { emptyEvent | action <- Event.NoOp , setContext <- Just Context.Default },
    cancel  = { emptyEvent | action <- Event.Dialog Dialog.Hide, setContext <- Just Context.Default }, 
    selectedOption = 0
    }

confirm : String -> Maybe Event.Event -> Maybe Event.Event -> Maybe (Dialog.Dialog Event.Event)
confirm query confirm cancel = let
    (Dialog.ConfirmDialog model) = emptyConfirmDialogModel
    in createDialog <| Dialog.ConfirmDialog
        { model | 
            query <- query,
            confirm <- Maybe.withDefault model.confirm confirm,
            cancel <- Maybe.withDefault model.cancel cancel
        }

---- DIALOG CREATION ----

createDialog : Dialog.Model Event.Event -> Maybe (Dialog.Dialog Event.Event) 
createDialog model = 
    case model of
        Dialog.ConfirmDialog m -> Just {
            model = model,
        
            update action dialModel = 
                let (Dialog.ConfirmDialog m') = dialModel
                in case action of
                    Dialog.ChangeSelection selection -> Dialog.ConfirmDialog { m' | selectedOption <- selection }
                    _ -> dialModel,
            
            view (w,h) address dialModel =
                let (Dialog.ConfirmDialog m') = dialModel
                in div [] [
                    div [
                        class "modal-background",
                        onClick address m'.cancel
                    ] [],
                    div [
                        class "confirm-dialog",
                        style [("top", toString (h//2 - 167) ++ "px"), ("left", toString (w//2 - 257) ++ "px")]
                    ] [
                        h2 [] [text m'.query],
                        div [] [
                            button [
                                classList [("confirm-button", m'.selectedOption == 0), ("cancel-button", m'.selectedOption /= 0)],
                                onClick address m'.confirm
                            ] [text "Confirm"],
                            button [
                                classList [("confirm-button", m'.selectedOption /= 0), ("cancel-button", m'.selectedOption == 0)],
                                onClick address m'.cancel
                            ] [text "Cancel"]
                        ]
                    ]
                ],
                
            keyboardInputMap keypresses dialModel =
                let 
                    (Dialog.ConfirmDialog m') = dialModel
                    confirmAction = (if m'.selectedOption == 0 then m'.confirm else m'.cancel)
                in case keypresses of
                    [13]     -> { emptyEvent | action <- Event.Dialog (Dialog.HideWith confirmAction) }
                    [27]     -> m'.cancel
                    [37]     -> { emptyEvent | action <- Event.Dialog (Dialog.Custom (Dialog.ChangeSelection 0)) }
                    [39]     -> { emptyEvent | action <- Event.Dialog (Dialog.Custom (Dialog.ChangeSelection 1)) }
                    _        -> emptyEvent
            }
        _ -> Nothing 
