> In the code-cleanup process, bugs have been introduced.  So this is a testing regimen for features.
> 
> - [x] Splitting a node
> 	- [x] Confirm commits
> 	- [x] Escape cancels
> - [x] Dragging a node
> 	- [x] Dragging follows the mouse coordinates
> 		- [x] Even when the graph is panned
> 	- [x] We can drag-and-pan
> - [x] Selection in the computation explorer loads the graph
> - [x] Creating a new computation places it at the head of the list
> - [x] Deleting a computation
> 	- [x] Can't delete the computation we're looking at right now
> - [x] Choosing destination for a node
> 	- [x] Node-styling is correct for phantom node
> 	- [x] Correct styling for phantom-node straight lines
> 		- [x] …and recursive lines
> 		- [x] …and non-straight lines
> 	- [x] Lock-on works at a proper lock-on distance
> 	- [x] Choosing destination follows the mouse
> 		- [x] Even when graph is panned
> 	- [x] We can pan-and-choose too
> 	- [x] Clicking makes the phantom node there
> 	- [x] Final node & line styling is proper, if I connection-edit and confirm the creation of a phantom-node
> 	- [x] I can escape out of making a phantom node
> - [x] Centering
> 	- [x] We re-center on the graph when we center
> 		- [x] Even when the graph node(s) have been dragged elsewhere
> - [ ] Panning
> 	- [x] We can't pan beyond the graph
> 	- [x] If there's no node in view, we can't pan away from the direction of some node(s)
> 	- [x] We can pan in
> 		- [x] the computation explorer
> 		- [x] the main window
> 		- [x] the connection editor "main" area
> 		- [x] the connection editor references
> 		- [x] the splitter interface
> - [x] Editing connection
> 	- [x] Input cycles properly through on-priority-off
> 	- [x] Reflects the updated connection in the graph, with proper priority + styling
> 	- [x] Removing all transitions removes the link in the graph view
> 	- [x] Connecting two existing nodes for the first time commits the new link (incl. self-loops) — added after bug #8
> - [x] Executing
> 	- [x] Stepping centers the graph
> 	- [x] Highlighting lines+arrows works during stepping
> 	- [x] Lowlight works properly
> 	- [x] Next-links are neither highlight nor lowlight
> - [x] Deleting a package
> 	- [x] Can't delete the main package
> - [x] Dragging a splitter
> 	- [x] Resizes the computation explorer and main window graphs properly
> 	- [x] Resizes the bottom window (during execution) and main window graphs properly
> 	- [x] Resizes the input-area and main-window graphs properly (not during execution)
> 		- [x] During description-entering too
> - [x] Clicking on connection text to start editing
> - [x] Undo/Redo
> 	- [x] Can undo, then replace with a new line
> 	- [x] Can undo-redo all the way from end to start and back
> 	- [x] Graph resizes/recenters properly during undo-redo
> - [x] Confirmation
> 	- [x] Can confirm at any point
> 	- [x] Confirmation optimizes the graph
> 	- [x] Confirmation centers the result properly
> 	- [x] Confirmation removes the undo-buffer
> - [x] Graph in general
> 	- [x] Nodes are displayed
> 	- [x] Straight lines where appropriate
> 	- [x] Curved lines where appropriate
> 	- [x] Recursive arrows where appropriate

## Event-decoder tests (added 2026-10-09)

Prerequisite: extract the decoders below into named, exposed values
(e.g. `keyboardDecoder : Model -> D.Decoder Msg`) and test them with
`D.decodeValue` on synthetic event JSON.  This covers the logic that decides
*which* `Msg`—if any—a DOM event produces, which is invisible to
update-simulation tests.  Malformed port payloads route to `CrashWithMessage`
(`Debug.todo`); that behaviour is documented here but is not testable.

- [x] Keyboard routing (model-dependent decoder)
	- [x] «Escape» is ignored unless `can.escape` is set
	- [x] «Ctrl-Escape» produces `Escape` during connection-editing and node-splitting; plain «Escape» is ignored there
	- [x] Plain «Escape» produces `Escape` in other escape-permitted states
	- [x] «Enter» produces `Confirm` in general
	- [x] «Ctrl-Enter» produces `Confirm` during connection-editing and node-splitting; plain «Enter» is ignored there
	- [x] «Ctrl-z» produces `Undo`; plain «z» is ignored
	- [x] «Ctrl-y» produces `Redo`; plain «y» is ignored
	- [x] Unmapped keys produce no message
- [x] Splitter-drag wiring
	- [x] LeftRight mouse-move applies the −52px navigator-bar offset to clientX before `DragSplitter`
	- [x] UpDown mouse-move passes clientY to `DragSplitter` unmodified
	- [x] Mouse-up produces the terminating `DragSplitter True` with the same offset convention
	- [x] No splitter messages are produced while no `DraggingSplitter` interaction is on the stack
- [x] Node-movement wiring
	- [x] Mouse-move during `DraggingNode` produces `MoveNode` with coordinates translated exactly once, using the host coordinates stored on the interaction
	- [x] Mouse-move during `ChoosingDestinationFor` likewise
	- [x] Mouse-up during `DraggingNode` produces `StopDraggingNode`
	- [x] No node-move messages are produced when neither interaction is on the stack
- [x] Pan ports
	- [x] A pan payload produces `Pan pan_x pan_y` for the encoded view uuid
	- [x] A stop-pan payload produces `StopPan` for the encoded view uuid
		- (pinned: the stopPan port delivers a *bare* uuid, not an object)
- [x] Graph-canvas handlers (decoders embedded in the view)
	- [x] Ctrl-click on a splittable node produces `StartSplit`
	- [x] Plain click on a node produces `SelectNode` with the event coordinates, when `can.selectNodes`
	- [x] Shift-mousedown on a node produces `StartDraggingNode` with the event coordinates, when `can.dragNodes`
	- [x] When the relevant capability is false, no message is produced

## Geometry assertions — Tier 1: semantic properties (added 2026-10-09)

Main suite; refactor-stable assertions on `DrawingData` / `LinkDrawingData`.

- [x] A one-way link has cardinality `Unidirectional`
- [x] A two-way non-recursive link has cardinality `Bidirectional`
- [x] A self-loop has cardinality `Recursive`
- [x] Source/target connection points of straight, curved, and recursive links lie on the boundaries of the respective node circles
	- (pinned precisely: the source sits on the circle (radius 7); the target sits 18 units out *by design*, to clear the arrowhead — see `tests/Tier1Geometry_tests.elm`)
- [x] CSS classes on nodes and link groups reflect model state (e.g. `phantom`, `final`, `terminal`, `start`, `selected`)
	- (pinned via the extracted pure functions `nodeClassFor` / `linkClassFor` / `characterTransitionClass` / `referenceTransitionClass`, which the views themselves use)

## Geometry assertions — Tier 2: exact pinning via golden fixtures (added 2026-10-09)

Exact `pathString` comparisons, with all expected values collected in a single
fixture module so that intentional geometry changes are regenerated in one place.

Note: SVG class attributes cannot be matched by `Test.Html.Selector` at all
(`Selector.attribute "class"` is special-cased into a property-based matcher
that only reads the HTML `className`; `svgClass` does **not** work either).
The class *computation* is therefore pinned through the extracted pure
functions (see Tier 1), and the DOM is pinned where the DOM is observable:
`d` attributes and element counts, scoped by each graph container's
`graph-id` attribute.

- [x] Golden fixture: full `pathString` for one straight link
- [x] Golden fixture: full `pathString` for one curved link
- [x] Golden fixture: full `pathString` for one recursive link
- [x] View wiring: the `d` attribute of a rendered link equals the model's `pathString` for that link
- [x] Regeneration procedure documented for intentional geometry changes
	- (in `tests/GoldenGeometry.elm`, module header)

## Testing tool (added 2026-10-09, Batch 17)

- [x] Writing a test input materialises the test (with an `ExpectAccepted` default) and recomputes its result
- [x] Results recompute as the input changes (accepted / rejected)
- [x] Emptying the input deletes the current test; the dangling test-key is harmless
- [x] Flipping the acceptance condition flips the expectation (and back)
- [x] Selecting or creating a test input stops an in-flight execution
- [x] Stopping execution this way resets the capabilities (found as bug #9)

## Layout messages (added 2026-10-09, Batch 18)

- [x] Resizing the window re-clamps the constants and re-derives the areas
- [x] Resizing the window re-hosts the graph views (found as bug #10: after a resize, click/drag coordinates were skewed by the stale host)
- [x] Collapsing/expanding the sidebar re-derives the main window (explorer views keep their hosts while closed — pinned)
- [x] Collapsing/expanding the bottom panel re-derives the main window
- [x] Splitter-drag messages outside a drag are ignored
- [x] QuickInput without a matching interaction is ignored

## Graph references (added 2026-10-09, Batch 19)

- [x] «`» enters reference-search mode; «`» again returns to character input
- [x] Typing filters the reference list by description (case-insensitive)
	- (the filter never worked in the real app: `toForceGraph` dropped `description = Nothing` on every solved view — found as bug #11)
- [x] Packages without a description always remain listed
- [x] Choosing a reference adds it to the connection; choosing again removes it
- [x] Confirming commits the reference into the graph edge
- [x] Reference labels carry the `graph-reference` classes (final / non-final)

## Miscellaneous handlers (added 2026-10-09, Batch 20)

- [x] Stop-pan resets the pan indicator; reset-pan re-centres the view (completes "Centering", including after nodes have been dragged elsewhere)
- [x] Tool and navigator selections are recorded; the computations icon refreshes the explorer (fresh views of the same packages)
- [x] Pan works in every area: computation explorer, main window, connection editor main area, connection editor references, splitter interface (completes "We can pan in")
- [x] Undo/Redo are refused while an interaction is in flight
- [x] Package messages for an unknown uuid are ignored

## Explicitly out of scope

- JS storage (`saveToStorage` / `deleteFromStorage` ports): not reachable from Elm tests; would require browser-level tests.
- Pan hover-trigger (JS → `pan` / `stopPan` ports): the JS side is untestable in Elm; the Elm side (Pan handling and pan limits) is covered under "Panning".
