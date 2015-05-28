module Planner.Component.Event where

import Planner.Event as Event
import Planner.Event exposing (..)
import Planner.UI.Context as Context
import Planner.UI.Dialog as Dialog
import Planner.UI.DialogBuilder as DialogBuilder

newProjectEvent : Event
newProjectEvent = 
    { emptyEvent |
        action <-
            Event.Dialog <|
            Dialog.Show  <|
            DialogBuilder.confirm
                "Are you sure you want to create a new project? Any unsaved progress will be lost."
                (Just { emptyEvent | action <- NewProject, setContext <- Just Context.Default })
                Nothing,
        setContext <- Just Context.Dialog 
    }

deleteNodeEvent : Event
deleteNodeEvent = 
    { emptyEvent |
        action <-
            Event.Dialog <|
            Dialog.Show  <|
            DialogBuilder.confirm
                "Are you sure you want to delete this item and all sub-items?"
                (Just { emptyEvent | action <- DeleteItem, setContext <- Just Context.Default })
                Nothing,
        setContext <- Just Context.Dialog 
    }
