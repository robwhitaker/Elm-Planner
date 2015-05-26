module Update where

import Model exposing (..)
import Tree as T
import Maybe

---- UPDATE ----

update : Input -> State dModel dAct -> State dModel dAct
update input s = let
        event = case input of
            KeyboardEvent keys -> keyPressesToEvent keys s
            UIEvent e          -> e

        ui' = s.ui
        oldContext = s.ui.context
        newContext = Maybe.withDefault s.ui.context event.setContext
        state = { s | ui <- { ui' | context <- newContext, 
                                    lastContext <- if newContext /= oldContext 
                                                   then oldContext 
                                                   else s.ui.lastContext,
                                    lastSelectedId <- s.selectedId
                            } 
                } 
        ui = state.ui

        confirmationDialog = state.ui.confirmationDialog

        selectableNodes = T.mapN (\(T.Node value children id) -> if not value.expanded then (T.Node value [] id) else (T.Node value children id)) state.rootNode
    in case event.action of
        NewItem -> let
            parent = T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode
            child = T.newNode state.rootNode newItem []
            newTree = T.addChildTo parent child state.rootNode
                      |> T.moveNode T.Lift child
                      |> T.valueM (\item -> { item | expanded <- True })
            in { state | rootNode <- newTree, selectedId <- T.id child }

        NewProject -> emptyModel

        SetAllExpanded expanded -> { state | rootNode <- T.map (\item -> { item | expanded <- expanded }) state.rootNode, selectedId <- if expanded then state.selectedId else 0 }

        LoadProject model -> model

        UpdateItemTitle newTitle -> { state | rootNode <- T.mapToNodeById (\item -> { item | title <- newTitle }) state.selectedId state.rootNode }

        Confirm dialog -> { state | ui <- { ui | confirmationDialog <- Just dialog, context <- ConfirmDialog } }

        Cancel -> { state | ui <- { ui | confirmationDialog <- Nothing, context <- Default } }

        ChangeConfirmSelection selection -> { state | ui <- { ui | confirmationDialog <- Maybe.map (\cd -> { cd | selectedOption <- selection } ) confirmationDialog } }

        SelectItem selectedId -> { state | selectedId <- selectedId }

        RenameProject newName -> { state | projectTitle <- newName }

        MoveSelection dir -> case dir of
            Up -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] 0) <| List.head <| List.reverse <| Utils.takeWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }
            Down -> { state | selectedId <- T.id <| Maybe.withDefault (T.Node newItem [] state.selectedId) <| List.head <| List.drop 1 <| Utils.dropWhile ((/=) state.selectedId << T.id) (T.flatten selectableNodes) }

        RenameItem newName -> 
            { state | rootNode <- T.mapToNodeById (\item -> { item | title <- newName }) state.selectedId state.rootNode, 
                      ui       <- { ui | context <- Default }
            }

        UpdateItem newContent ->
            { state | rootNode <- T.mapToNodeById (\item -> { item | content <- newContent }) state.selectedId state.rootNode }

        RenamingItem selectedId -> case selectedId of
            Just sId -> { state | selectedId <- sId, ui <- { ui | context <- RenamingNode } }
            Nothing  -> { state | ui <- { ui | context <- RenamingNode } }

        DeleteItem -> let
                previousNode = Maybe.withDefault (T.dummyNode newItem) <| List.head <| List.reverse 
                               <| Utils.takeWhile ((/=) (T.nodeByIdWithDefault (T.dummyNode newItem) state.selectedId state.rootNode)) (T.flatten selectableNodes)
                prevId = if T.id previousNode < 0 then 0 else T.id previousNode
            in { state | rootNode <- T.removeNodeById state.selectedId state.rootNode, 
                         selectedId <- prevId, 
                         ui <- { ui | confirmationDialog <- Nothing }
               }

        MoveNode movement -> case movement of
            T.Lower -> let 
                newState = { state | rootNode <- T.moveNodeById movement state.selectedId state.rootNode }
                parent = T.parent (T.nodeByIdWithDefault state.rootNode state.selectedId newState.rootNode) newState.rootNode |> Maybe.withDefault state.rootNode
                in { newState | rootNode <- T.mapToNodeById (\item -> { item | expanded <- True }) (T.id parent) newState.rootNode }

            _       -> { state | rootNode <- T.moveNodeById movement state.selectedId state.rootNode }

        ToggleExpanded selectedId -> case selectedId of
            Just sId -> let
                toggledNode = T.nodeByIdWithDefault (T.dummyNode newItem) sId state.rootNode
                in case T.nodeById state.selectedId toggledNode of
                    Just _ -> 
                        { state | 
                            rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) sId state.rootNode,
                            selectedId <- sId 
                        }
                    Nothing -> { state | rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) sId state.rootNode }
            Nothing  -> 
                { state | rootNode <- T.mapToNodeById (\item -> { item | expanded <- not item.expanded }) state.selectedId state.rootNode }

        _ -> state

---- KEYPRESS MAP ----

keyPressesToEvent : List Int -> State dModel dAct -> Event
keyPressesToEvent keypresses state = case keypresses of
    [17, 191] -> newProjectEvent
    _ -> case state.ui.context of
        Default -> case keypresses of 
            [17, 37] -> { emptyEvent | action <- MoveNode T.Lift }
            [17, 38] -> { emptyEvent | action <- MoveNode T.ShiftUp }
            [17, 39] -> { emptyEvent | action <- MoveNode T.Lower }
            [17, 40] -> { emptyEvent | action <- MoveNode T.ShiftDown }
            [13, 17] -> { emptyEvent | action <- NewItem }
            [46]     -> deleteNodeEvent
            [32]     -> { emptyEvent | action <- ToggleExpanded Nothing }
            [13]     -> { emptyEvent | action <- RenamingItem Nothing }
            [38]     -> { emptyEvent | action <- MoveSelection Up }
            [40]     -> { emptyEvent | action <- MoveSelection Down }
            [27]     -> { emptyEvent | setContext <- Just TitleInput }
            [9]      -> { emptyEvent | setContext <- Just MainTextArea }
            _        -> emptyEvent

        ConfirmDialog -> let
            dialog = Maybe.withDefault emptyDialog state.ui.confirmationDialog
            confirmAction = (if dialog.selectedOption == 0 then .confirm else .cancel) dialog
            in case keypresses of
                [13]     -> { emptyEvent | action <- confirmAction, setContext <- Just Default }
                [27]     -> { emptyEvent | action <- dialog.cancel, setContext <- Just Default }
                [37]     -> { emptyEvent | action <- ChangeConfirmSelection 0 }
                [39]     -> { emptyEvent | action <- ChangeConfirmSelection 1 }
                _        -> emptyEvent

        TitleInput -> case keypresses of
            [9]      -> { emptyEvent | setContext <- Just state.ui.lastContext }
            [13]     -> { emptyEvent | setContext <- Just state.ui.lastContext }
            [27]     -> { emptyEvent | setContext <- Just state.ui.lastContext }
            _        -> emptyEvent

        MainTextArea -> case keypresses of
            [9]          -> { emptyEvent | setContext <- Just Default }
            [27]         -> { emptyEvent | setContext <- Just TitleInput }
            _        -> emptyEvent

        RenamingNode -> case keypresses of
            [9]          -> { emptyEvent | setContext <- Just MainTextArea }
            [13]         -> { emptyEvent | setContext <- Just Default }
            [38]         -> { emptyEvent | setContext <- Just Default }
            [40]         -> { emptyEvent | setContext <- Just Default }
            _            -> emptyEvent

        _ -> emptyEvent