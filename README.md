# Elm-Planner
A recursive planner application written in [Elm](http://elm-lang.org).

Just want to use it? [Try it out now!](http://robwhitaker.github.io/Elm-Planner)

## Features
* Take notes in a tree-hierarchy, allowing each node to have sub-nodes down to any depth.
* Each node can be titled and can store text content of its own (including nodes with children).
* Autosave as you go! Every change you make is automatically saved to localstorage.
* Save projects to or load project from your local machine. Working from multiple computers or browsers? Save the file, transfer it to your other machine, and upload it right back into Elm Planner!

## Building Locally

This is assuming you have Elm v0.15+ installed already. If not, get it [here](http://elm-lang.org).

1. Clone this repository.
2. In terminal:
  ```
  $ cd PATH_TO_ELM_PLANNER
  
  $ elm-package install -y
  
  $ ./make && elm reactor
  ```
3. Open `localhost:8000/index.html` in your browser.
