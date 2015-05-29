module Planner.Component.Event where

import Planner.Event as Event
import Planner.Event exposing (..)
import Planner.UI.Context as Context
import Planner.UI.Dialog as Dialog
import Planner.UI.DialogBuilder as DialogBuilder
import Planner.Component.Text as Text

newProjectEvent : Event
newProjectEvent = 
    { emptyEvent |
        action <-
            Event.Dialog <|
            Dialog.Show  <|
            DialogBuilder.confirm
                Text.confirmNewProject
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
                Text.confirmDeleteNode
                (Just { emptyEvent | action <- DeleteItem, setContext <- Just Context.Default })
                Nothing,
        setContext <- Just Context.Dialog 
    }
