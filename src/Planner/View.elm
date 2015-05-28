module Planner.View where

import Planner.Model exposing (..)
import Planner.UI.Dialog as Dialog
import Planner.UI.Context as Context
import Planner.Data.Tree exposing (Tree)
import Planner.Data.Tree as T
import Planner.Event as Event
import Planner.Event exposing (..)
import Planner.Component.Event exposing (newProjectEvent, deleteNodeEvent)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Signal
import Maybe exposing (andThen)
import Markdown

---- RENDER FUNCTIONS ----

render : Signal.Address Event.Event -> State -> (Int, Int) -> Html
render address state (w, h) = let
        dialog = 
            Maybe.withDefault 
                (div [] []) 
                <| state.ui.dialog `andThen` (\dialog -> Just <| dialog.view (w,h) address dialog.model) 
    in div [class "wrapper"] [
        dialog,
        div [class "title-bar"] [
            input [
                id "title-bar-input",
                if state.ui.context == Context.TitleInput then style [] else value state.projectTitle,
                type' "text", 
                on "input" targetValue (Signal.message address << (\act ->
                    { emptyEvent | action <- act }
                ) << RenameProject),
                onFocus address { emptyEvent | setContext <- Just Context.TitleInput },
                placeholder "Untitled Project"
            ] []
        ],
        div [class "options-bar", onClick address {emptyEvent | setContext <- Just Context.Default } ] [
            i [class "fa fa-file-text-o icon", onClick address newProjectEvent, alt "New Project (Ctrl+/)", title "New Project (Ctrl+/)"] [],
            -- TODO : address was saveFile.address (fix wiring later)
            i [class "fa fa-download icon", onClick address { emptyEvent | action <- SaveProject }, alt "Save Project  (Ctrl+S)", title "Save Project (Ctrl+S)"] [],
            label [for "loadButton"] [
                Html.form [id "loadWrapperForm"] [ 
                    input  [type' "file", id "loadButton"] [text "Load"],
                    i [class "fa fa-upload icon", alt "Load Project (Ctrl+O)", title "Load Project (Ctrl+O)"] []
                ]
            ],
            div [class "divider"] [],
            i [class "fa fa-plus-square-o icon", onClick address { emptyEvent | action <- NewItem, setContext <- Just Context.Default }, alt "New Node (Ctrl+Return)", title "New Node (Ctrl+Return)" ] [], 
            i [class "fa fa-pencil icon", onClick address { emptyEvent | action <- RenamingItem Nothing }, alt "Rename Node (Return)", title "Rename Node (Return)"] [],
            i [class "fa fa-arrows-v icon", onClick address { emptyEvent | action <- ToggleExpanded Nothing }, alt "Toggle Expanded (Space)", title "Toggle Expanded (Space)"] [],
            i [class "fa fa-trash-o icon", onClick address deleteNodeEvent, alt "Delete Node (Del)", title "Delete Node (Del)" ] [], 
            div [class "divider"] [],
            i [class "fa fa-arrow-left icon", onClick address { emptyEvent | action <- MoveNode T.Lift, setContext <- Just Context.Default }, alt "Move Left (Ctrl+Left)", title "Move Left (Ctrl+Left)" ] [], 
            i [class "fa fa-arrow-up icon", onClick address { emptyEvent | action <- MoveNode T.ShiftUp, setContext <- Just Context.Default }, alt "Move Up (Ctrl+Up)", title "Move Up (Ctrl+Up)" ] [], 
            i [class "fa fa-arrow-down icon", onClick address { emptyEvent | action <- MoveNode T.ShiftDown, setContext <- Just Context.Default }, alt "Move Down (Ctrl+Down)", title "Move Down (Ctrl+Down)" ] [], 
            i [class "fa fa-arrow-right icon", onClick address { emptyEvent | action <- MoveNode T.Lower, setContext <- Just Context.Default }, alt "Move Right (Ctrl+Right)", title "Move Right (Ctrl+Right)" ] [],
            div [class "divider"] [],
            i [class "fa fa-caret-square-o-up icon", onClick address { emptyEvent | action <- SetAllExpanded False, setContext <- Just Context.Default }, alt "Collapse All", title "Collapse All" ] [],
            i [class "fa fa-caret-square-o-down icon", onClick address { emptyEvent | action <- SetAllExpanded True, setContext <- Just Context.Default }, alt "Expand All", title "Expand All" ] []
        ],
        div [
            class "main-container",
            style [("height", toString (h-140) ++ "px")]
        ] [
            div [
                class "tree-pane",
                onClick address { emptyEvent | setContext <- Just Context.Default }
            ] [lazy3 treeToHtmlTree address state state.rootNode],
            label [for "textbox"] [
                div [
                    class "text-area-container"
                ] [
                    textarea [
                        id "textbox",
                        placeholder "...",
                        on "input" targetValue (Signal.message address << (\act -> 
                            { emptyEvent | action <- act }
                        ) << UpdateItem),
                        onFocus address { emptyEvent | setContext <- Just Context.MainTextArea }
                    ] []
                ]
            ]
        ],
        footer [] [
            Markdown.toHtml """ Created by <a href="http://robertjwhitaker.com" target="_blank">Robert J. Whitaker</a>

*This project was programmed in <a href="http://elm-lang.org" target="_blank">Elm</a>. Check out the <a href="https://github.com/robwhitaker/Elm-Tree-Planner" target="_blank">source</a>.*
"""
        ]
    ]

treeToHtmlTree : Signal.Address Event.Event -> State -> Tree Item -> Html
treeToHtmlTree address state (T.Node item children id') = let
        liContent = if state.ui.context == Context.RenamingNode && id' == state.selectedId
                    then 
                        input [ 
                            type' "text",
                            id ("node-" ++ toString id'), 
                            onClick address { emptyEvent | action <- SelectItem id', setContext <- Just Context.RenamingNode }, 
                            on "input" targetValue (Signal.message address << (\act -> { emptyEvent | action <- act }) << UpdateItemTitle)
                        ] []
                    else 
                        div [
                            id ("node-" ++ toString id'),
                            classList [
                                ("item-title", True),
                                ("selected-focused", id' == state.selectedId && state.ui.context == Context.Default), 
                                ("selected-unfocused", id' == state.selectedId && (not <| state.ui.context == Context.Default))
                            ],
                            onClick address { emptyEvent | action <- SelectItem id', setContext <- Just Context.Default }, 
                            onDoubleClick address { emptyEvent | action <- RenamingItem (Just id') }
                        ] [text item.title]
        in ul [classList [("root-node", id' == 0)]] [
            li [
                classList [("hidden", not item.expanded && children /= [])]
            ] <| 
                div [class "arrow-container"] [
                    img [
                        class "expand-arrow-icon",
                        src (if item.expanded then "img/arrow-expanded.png" else "img/arrow-collapsed.png"), 
                        onClick address { emptyEvent | action <- ToggleExpanded (Just id') }
                    ] []
                ] 
                :: liContent 
                :: (if item.expanded then List.map (lazy3 treeToHtmlTree address state) children else [])]
