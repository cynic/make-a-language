port module Ports exposing (..)
import Json.Encode as E

port saveToStorage : E.Value -> Cmd msg
port deleteFromStorage : String -> Cmd msg

{-
At the start of the program, load all records from the IndexedDB
database into the Elm app, using `flags`. During program runtime,
saving works as follows:

Saved values and loaded values are JSON objects, so I need to make
functions that will encode and decode a GraphPackage.

I also need to modify the Model in Main.elm so that it keeps track
of all the computations (i.e. the GraphPackage values).  Only one
of these computations must be the current one.  Initially, that is
a "newly created" computation and the code for this is already in
the `init` function of Main.elm.  All the remaining computations
should be displayed in the "Computations" panel panel as a column
of SVG images; one can see how to convert an `AutomatonGraph a`
into an SVG value by studying the `view` function in
ForceDirectedGraph.elm.

A user should be able to open the "Computations" panel and select
any computation, which then becomes the `currentPackage`.

There must be a keypress or button to save the graph that is being
worked on.  That saving should be done via the `saveToStorage` port,
and the JavaScript code for this is in `index.html`.  The saved
status of a computation/graph must be shown in the status bar.  If
a user tries to move away from a graph that has unsaved changes, a
`modal` is used to double-check that choice and either save or
confirm the discarding of changes.

Tests for the `currentPackage` appear in the "Tests" panel.  Any
test which is selected appears in the "Add Tests" panel for editing.
When a test is saved, it appears in the list of tests.
-}