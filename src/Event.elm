module Event where

import Context exposing (..)

---- ADTs ----

type Action 
    = NewItem
    | SetAllExpanded Bool
    | Confirm Dialog
    | Cancel
    | ChangeConfirmSelection Int
    | NewProject
    | UpdateItemTitle String
    | LoadProject String -- this used to take State; fix elsewhere.
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