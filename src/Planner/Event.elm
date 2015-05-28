module Planner.Event where

import Planner.UI.Context exposing (Context)
import Planner.Data.Tree exposing (NodeMovement)
import Planner.UI.Dialog as Dialog

---- ADTs ----

type Action
    = NewItem
    | SetAllExpanded Bool
    | Dialog (Dialog.Action Event)
    | NewProject
    | UpdateItemTitle String
    | SaveProject
    | LoadProject String
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


type Input = KeyboardEvent (List Int) | UIEvent Event

---- EVENT MODEL ----

type alias Event = {
    action : Action,
    setContext : Maybe Context
}

emptyEvent : Event 
emptyEvent = {
    action = NoOp,
    setContext = Nothing
    }

