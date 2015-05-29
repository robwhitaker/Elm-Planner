module Planner.Component.Text where

---- CONFIRMATION TEXT ----

confirmNewProject = "Are you sure you want to create a new project? Any unsaved progress will be lost."

confirmDeleteNode = "Are you sure you want to delete this item and all sub-items?"

confirmLoadProject = "Are you sure you want to load a new project? Any unsaved progress will be lost."

---- NEW MODEL TEXT ----

newItemTitle = "new"

---- KEYBOARD SHORTCUTS ----

newProjectShortcut = ("New Project", "Ctrl+/")

saveProjectShortcut = ("Save Project", "Ctrl+S")

loadProjectShortcut = ("Load Project", "Ctrl+O")

newNodeShortcut = ("New Node", "Ctrl+Return")

renameNodeShortcut = ("Rename Node", "Return")

toggleExpandedShortcut = ("Toggle Expanded", "Space")

deleteNodeShortcut = ("Delete Node", "Del")

moveNodeLeftShortcut = ("Move Node Left", "Ctrl+LeftArrow")

moveNodeUpShortcut = ("Move Node Up", "Ctrl+UpArrow")

moveNodeDownShortcut = ("Move Node Down", "Ctrl+DownArrow")

moveNodeRightShortcut = ("Move Node Right", "Ctrl+RightArrow")

moveSelectionUpShortcut = ("Move Selection Up", "UpArrow")

moveSelectionDownShortcut = ("Move Selection Down", "DownArrow")

editTitleShortcut = ("Focus Title", "Esc")

toggleFocusShortcut = ("Change Focus", "Tab")

showHelpShortcut = ("Show Keyboard Shortcuts", "h")

---- BUTTON ALT TEXT ----

scToAlt (actionStr, keyStr) = actionStr ++ " (" ++ keyStr ++ ")" 

newProjectButtonAlt = scToAlt newProjectShortcut

saveProjectButtonAlt = scToAlt saveProjectShortcut

loadProjectButtonAlt = scToAlt loadProjectShortcut

newNodeButtonAlt = scToAlt newNodeShortcut

renameNodeButtonAlt = scToAlt renameNodeShortcut

toggleExpandedButtonAlt = scToAlt toggleExpandedShortcut

deleteNodeButtonAlt = scToAlt deleteNodeShortcut

moveNodeLeftButtonAlt = scToAlt moveNodeLeftShortcut

moveNodeUpButtonAlt = scToAlt moveNodeUpShortcut

moveNodeDownButtonAlt = scToAlt moveNodeDownShortcut

moveNodeRightButtonAlt = scToAlt moveNodeRightShortcut

collapseAllButtonAlt = "Collapse All"

expandAllButtonAlt = "Expand All"

showHelpButtonAlt = scToAlt showHelpShortcut

---- MISC ----

untitledSaveTitle = "untitled"

untitledProjectPlaceholderTitle = "Untitled Project"

emptyTextAreaPlaceholder = "..."

---- MARKDOWN ----

footerText = 
    """ Created by <a href="http://robertjwhitaker.com" target="_blank">Robert J. Whitaker</a>

*This project was programmed in <a href="http://elm-lang.org" target="_blank">Elm</a>. Check out the <a href="https://github.com/robwhitaker/Elm-Tree-Planner" target="_blank">source</a>.*
"""
