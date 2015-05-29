module Planner.UI.DialogBuilder where

import Planner.UI.Dialog as Dialog
import Planner.Event as Event
import Planner.Event exposing (emptyEvent)
import Planner.UI.Context as Context
import Planner.Component.Text as Text

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import List 

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

---- KEYBOARD SHORTCUT DIALOG ----

keyboardShortcutDialogModel : Dialog.Model Event.Event
keyboardShortcutDialogModel = Dialog.KeyboardShortcutDialog {
        keyMap = [
                ("FILE", ""),
                Text.newProjectShortcut,
                Text.saveProjectShortcut,
                Text.loadProjectShortcut, 

                ("NODE OPERATIONS", ""),
                Text.newNodeShortcut,
                Text.renameNodeShortcut,
                Text.toggleExpandedShortcut,
                Text.deleteNodeShortcut,

                ("NODE MOVEMENT", ""),
                Text.moveNodeLeftShortcut,
                Text.moveNodeUpShortcut,
                Text.moveNodeDownShortcut,
                Text.moveNodeRightShortcut,

                ("SELECTION MOVEMENT", ""),
                Text.moveSelectionUpShortcut,
                Text.moveSelectionDownShortcut,

                ("CHANGING FOCUS", ""),
                Text.editTitleShortcut,
                Text.toggleFocusShortcut,

                ("HELP MENU", ""),
                Text.showHelpShortcut
            ]
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
                        class "dialog",
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

        Dialog.KeyboardShortcutDialog m -> Just {
            model = model,

            update action dialModel = dialModel,

            keyboardInputMap keypresses dialModel = 
                case keypresses of
                    [27] -> { emptyEvent | action <- Event.Dialog Dialog.Hide, setContext <- Just Context.Default }
                    [72] -> { emptyEvent | action <- Event.Dialog Dialog.Hide, setContext <- Just Context.Default }
                    _ -> emptyEvent,

            view (w,h) address dialModel =
                let (Dialog.KeyboardShortcutDialog m') = dialModel
                in div [] [
                    div [
                        class "modal-background",
                        onClick address { emptyEvent | action <- Event.Dialog Dialog.Hide, setContext <- Just Context.Default }
                    ] [],
                    div [
                        class "dialog keyboard-shortcuts",
                        style [("top", toString ((toFloat h * 0.5) - (toFloat h * 0.8)/2) ++ "px"), ("left", toString (w//2 - 250) ++ "px"), ("height", toString (toFloat h * 0.8) ++ "px")]
                    ] [
                        ul [] <|
                            List.foldr (\(actionStr, keyStr) acc -> let
                                    color = (List.length acc) % 2
                                    tag = if keyStr == "" 
                                          then li [class "heading"] [h4 [] [text actionStr]] 
                                          else li [class <| "color-" ++ toString color] [div [class "left-side"] [text actionStr], div [class "right-side"] [text keyStr]]
                                    in tag :: acc
                                ) [] m'.keyMap
                    ]
                ]
            }

        _ -> Nothing 
