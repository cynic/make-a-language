# UI Test Batches

Plan for implementing the testing regimen in `UI-Tests.md` (see that file for
the feature checklist and the added event-decoder/geometry sections).

## Conventions

- Each batch is one commit; the suite (`npm test` → `elm-test-rs`) must be
  green after each batch, and `elm make` must compile cleanly.
- Known-failing tests (bugs from the code-cleanup process) are written but
  wrapped in `Test.skip`, with a comment naming the bug.  Un-skipping the test
  is then the verification of the bugfix.
- Test-only batches (2–12) touch no app code.  App-code refactors (13–16) come
  last, so behaviour is pinned by tests *before* anything in `Main.elm` or
  `GraphEditor.elm` moves.
- `Test.Html.Query`/`Selector` come from `elm-explorations/test` 2.2.0 (already
  a direct dependency; the elm-html-test functionality was merged into it).
  **SVG gotcha**: `Selector.class` matches only the `className` property, but
  SVG classes are class *attributes* in this codebase — use the harness's
  `svgClass` selector for SVG elements.

## Batches

- [x] **Batch 1 — Scaffolding**
  Append the new sections to `UI-Tests.md`; create `tests/UILibrary.elm`
  (flags builder → `defaultModel` via `Main.init`, `withGraph` helper, msg
  steppers, view-query helper, `svgClass` selector helper, fixed UUIDs); smoke
  test that init yields the expected initial model.
  Also: fixed `npm test` — pnpm's bin shim runs the elm-test-rs *native
  binary* (hard-linked over its JS launcher at install) through node; the
  script now execs the binary directly via `scripts/test.sh`.
  *Commit:* `Test scaffolding: UI harness`
- [x] **Batch 2 — Pure geometry functions**
  `translateHostCoordinates` / `calculate_host_coord` (incl. panned cases);
  `fitGraphViewToGraph` centering; `panGraphView` limits + heading heuristic.
  *Covers:* "Even when the graph is panned", centering, pan-limit items.
  *Commit:* `UI tests: coordinate translation, centering, pan limits`
- [x] **Batch 3 — Dragging a node**
  Follows mouse; works when panned; drag-and-pan (capabilities hold mid-drag).
  *Commit:* `UI tests: dragging nodes`
- [x] **Batch 4 — Choosing destination**
  Push/replace of `ChoosingDestinationFor`; follows mouse (panned & not);
  pan-and-choose; click places phantom; Escape cancels; lock-on distance.
  *First known-fail found:* Escape during existing-node selection never popped
  the interaction (Main.elm Escape handler, `ExistingNode` branch passed the
  un-popped model to `removePhantomLink`) — **fixed** in the same batch:
  the branch now pops via `model_` and calls `setCapabilities`.
  *Commit:* `UI tests: choosing destination for a node` (+ bugfix)
- [x] **Batch 5 — Splitting a node**
  Drive split-interface msgs; Confirm commits (assert via `ag_equals`);
  Escape cancels and restores.
  *Second known-fail found:* confirming a split with an empty side cancelled
  the interaction but left stale split-mode capabilities — `commitOrConfirm`'s
  cancel branch never called `setCapabilities`.  **Fixed** in the same batch.
  *Commit:* `UI tests: splitting a node` (+ bugfix)
- [x] **Batch 6 — Connection editing**
  `EditConnection` flow; input cycles on-priority-off; removing all transitions
  removes link; updated connection reflected (model-level).
  *Third known-fail found:* `commit_change` never recalculated `drawingData`,
  so the canvas kept rendering the pre-commit link after any confirmed edit
  (stale connection, and an emptied link still shown).  **Fixed** in the same
  batch by piping the updated view through `recalculateLinksAndNodes`.
  *Also pinned:* the optimisation round-trip removes an emptied link *and*
  its now-disconnected node — deliberate behaviour, asserted as such.
  *Commit:* `UI tests: connection editing` (+ bugfix)
- [x] **Batch 7 — Executing**
  Step centers graph; highlight/lowlight sets; next-links neutral
  (all `drawingData` assertions).
  *Semantics pinned (behaviour, not bugs):* node ids are renumbered per
  execution, so assertions are structural; DFA minimisation merges
  equivalent states (the test graph gives the alternative branch an
  outgoing edge to keep it distinct); the final step's view contains
  exactly the executed path; lowlighting is asserted at step 1, where the
  full graph is shown.
  *Commit:* `UI tests: executing computations`
- [x] **Batch 8 — Splitter & layout**
  `StartDraggingSplitter` → `DragSplitter` through the three layout contexts
  (explorer/main, bottom-window during execution, input-area + description
  variant); `uiLayout.dimensions` assertions.
  *Fourth known-fail found:* view capabilities were not frozen during
  splitter drags — `DraggingSplitter` is keyed globally (`Nothing`), so the
  all-false `whenDraggingSplitter` view caps (which existed in the code!)
  could never apply, since `setLocalCapabilities` only consulted per-view
  keys.  **Fixed** by making the `Nothing` branch consult the global
  most-recent interaction.
  *Commit:* `UI tests: splitter dragging and layout` (+ bugfix)
- [x] **Batch 9 — Package management**
  Explorer selection loads graph; new computation at head; delete computation;
  can't delete current; can't delete main package.
  *Fifth known-fail found:* `beginDeletionInteraction` pushed the
  `DeletingPackage` interaction without calling `setCapabilities`, so
  `can.escape` never became True and the deletion-confirm screen could only
  be confirmed, never cancelled (`whenDeletingPackage`'s escape=True was
  unreachable).  **Fixed** in the same batch.
  *Commit:* `UI tests: computation explorer and packages` (+ bugfix)
- [x] **Batch 10 — Undo/Redo & Confirmation**
  Undo/redo chains incl. undo-then-new-clears-redo; recentering during
  undo/redo; confirm at any point; confirm optimizes, centers, clears buffers.
  *Two known-fails found:*
  6. `confirmPhantomNode` set `computation = ag` on the view *before*
     calling `commit_change`, so the undo buffer received the post-commit
     graph and Undo after phantom-node creation was a graphical no-op —
     fixed by passing the un-mutated view.
  7. Undo/Redo never refreshed `drawingData` or refit the viewport, so an
     undone connection edit kept drawing the committed (final) styling —
     fixed by piping through `recalculateLinksAndNodes` + `fitGraphViewToGraph`.
  *Commit:* `UI tests: undo/redo and confirmation` (+ 2 bugfixes)
- [x] **Batch 11 — Tier-1 geometry (model-level)**
  Cardinality (uni/bi/recursive); connection points on node circle boundaries;
  state classes via `nodeDrawingForPackage`.
  *Semantics pinned (behaviour, not bugs):* the source connection point sits
  exactly on the node circle (radius 7); the target sits 18 units out —
  deliberately clear of the boundary to make room for the arrowhead; the
  recursive loop's base midpoint is exactly on the circle; `isTerminal`
  means "has a *final* incoming transition" (not "no outgoing").
  *Commit:* `UI tests: Tier-1 geometry assertions`
- [x] **Batch 12 — View styling + Tier-2 goldens**
  Phantom node/line styling; classes (`phantom`, `final`, `terminal`,
  `start`, `selected`); golden-fixture module with the three pinned
  `pathString`s + regeneration procedure; `d`-attribute wiring.
  *Refactor (user-approved extraction pattern):* SVG class attributes
  cannot be matched by `Test.Html.Selector` at all (`Selector.attribute
  "class"` is special-cased into a property-based matcher reading
  `className`), so `nodeClassFor`, `linkClassFor`,
  `characterTransitionClass` and `referenceTransitionClass` were extracted
  from `GraphEditor`'s view code as pure functions (used by the views) and
  pinned that way — exact ordered class lists, stronger than attribute
  matching.  DOM-level pinning remains where the DOM is observable: `d`
  attributes and node counts (scoped by the `graph-id` attribute).
  *Semantics pinned:* the original real link stays drawn while a phantom
  is chosen (only the tentative self-link is replaced); the lowlight
  *class* is looked up on the REVERSED pair (incoming-side dimming).
  *Commit:* `UI tests: view styling and golden geometry fixtures` (+ extraction)
- [x] **Batch 13 — Keyboard decoder**
  Extract `keyboardDecoder : Model -> D.Decoder Msg` from `subscriptions`; test
  all routing rules with synthetic event JSON.
  *Commit:* `Extract & test keyboard routing decoder`
- [x] **Batch 14 — Splitter + pan-port decoders**
  Extract splitter mouse-move/mouse-up decoders (incl. the −52px offset) and
  the two port decoders; test.
  *Semantics pinned:* the `stopPan` port delivers a bare uuid (not an
  object); malformed payloads fail the decoders and are routed to
  `CrashWithMessage` by the subscription.
  *Commit:* `Extract & test splitter and pan-port decoders`
- [x] **Batch 15 — Node-movement wiring**
  Extract the `createNodeMoveSubscription` decoder (single-translation
  invariant, `StopDraggingNode`); test.
  *Commit:* `Extract & test node-movement wiring`
- [x] **Batch 16 — Canvas handlers**
  Extract the ctrl-click / shift-mousedown / click decoders from
  `GraphEditor.viewNode` (capability gates folded into the decoders, so
  "no message when denied" is testable) and `linkClickMsg` from `viewLink`;
  test directly with synthetic JSON, plus integration checks that each
  extracted message drives its real flow.
  *Commit:* `Extract & test graph-canvas event decoders`

- [x] **Batch 17 — Test-input management** (`tests/TestInputManagement_tests.elm`)
  `CreateNewTestInput`, `SelectTest`, `DeleteTestInput`, `UpdateTestInput`
  (incl. the empty-input-deletes rule and result recomputation),
  `FlipAcceptanceCondition`; the execution-stop interplay.
  *Bug #9 found:* both stop-execution branches of `update_package` omitted
  `setCapabilities` (bug #5's twin: stale `whenExecuting` capabilities after
  execution ends) — fixed by appending `UI.setCapabilities model_`.
  *Commit:* `UI tests: test-input management` (+ bugfix)
- [x] **Batch 18 — Layout messages** (`tests/LayoutMessages_tests.elm`)
  `OnResize` (constant clamping + area re-derivation), `ToggleAreaVisibility`,
  plus the `DragSplitter`-outside-drag and unmatched-`QuickInput` guards.
  *Bug #10 found:* `resizeViewport` never re-hosted the graph views and the
  `OnResize` handler didn't either — stale `host` skewed every subsequent
  click/drag coordinate translation — fixed by piping the handler through
  `updateMainEditorDimensions`.
  *Commit:* `UI tests: layout messages` (+ bugfix)
- [x] **Batch 19 — Graph-reference flow** (`tests/GraphReference_tests.elm`)
  Search mode enter/exit, description filtering (incl. the no-description
  rule), `ViaGraphReference` toggling round-trip, confirm commits the
  reference, `referenceTransitionClass`.
  *Bug #11 found:* `toForceGraph` hardcoded `description = Nothing`, so every
  force-solved view lost its description — the reference-search filter could
  never filter anything in the real app (the optimisation round-trip had
  already worked around this same loss) — fixed by preserving the
  description.
  *Commit:* `UI tests: graph-reference flow` (+ bugfix)
- [x] **Batch 20 — Miscellaneous handlers** (`tests/MiscMessages_tests.elm`)
  `StopPan`/`ResetPan` handlers (completes Centering), `SelectTool`/
  `SelectNavigation` (+ explorer refresh), pan-in-every-area matrix
  (completes "We can pan in"), undo/redo mid-interaction refusal,
  unknown-uuid package no-op.
  *Commit:* `UI tests: miscellaneous handlers`
- Deferred at the user's request: `SelectDebugStep` (under active design in
  that area — the `ExecutionData.debugging` expansion views).

## Status

- **All 20 batches completed** (`SelectDebugStep` deferred by design).  Suite: **421 tests, all green** (no skips).  `elm make` clean.
- Runner note: `scripts/test.sh` treats elm-test-rs's "incomplete" exit code (2, caused by `Test.skip`) as success when there are zero failures.
- Coverage-gap lesson (bug #8): the click-flow tests treated "the right interaction was pushed" as the finish line, and the commit tests started from the `EditConnection` message (which hardcodes `ExistingNodeExistingConnection`) — so the `ExistingNodeNewConnection` commit path had no end-to-end coverage.  The checklist has no item for the (source × targetKind) matrix of the connection editor; the new item under "Editing connection" now records it.
- Bugs found and fixed (eleven):
  1. Escape during existing-node selection never popped the interaction or reset capabilities (Main.elm Escape handler `ExistingNode` branch).
  2. Confirming a node-split with an empty side left stale split-mode capabilities (`commitOrConfirm` cancel branch).
  3. `commit_change` never recalculated `drawingData` — stale links after every confirmed edit.
  4. View capabilities were never frozen during splitter drags (global `DraggingSplitter` invisible to `setLocalCapabilities`).
  5. `beginDeletionInteraction` never called `setCapabilities` — the deletion-confirm screen could not be escaped.
  6. `confirmPhantomNode` pushed the post-commit graph onto the undo buffer — Undo after phantom creation was a no-op.
  7. Undo/Redo never refreshed `drawingData` or refitted the viewport.
  8. `commitOrConfirm`'s `ExistingNodeNewConnection` branch called `removeLink` (which only empties an existing edge, and is a no-op when there is none) instead of creating the link with its edited transitions — a *new* connection between existing nodes was silently swallowed (found by the user's "(ab)+" flow; regression tests drive both the lock-on variant and the self-loop variant end-to-end).
  9. `update_package`'s stop-execution branches (`CreateNewTestInput`/`SelectTest`) omitted `setCapabilities` — stale `whenExecuting` capabilities after execution ended (bug #5's twin).
  10. `OnResize` never re-hosted the graph views — after a window resize, every click/drag coordinate translation was skewed by the stale `host`.
  11. `toForceGraph` hardcoded `description = Nothing` — every force-solved view lost its description, so the connection editor's reference search could never filter anything in the real app (the optimisation round-trip had already worked around this same loss).
- App-code extractions for testability (each used by the production code): class-list functions (`nodeClassFor`, `linkClassFor`, `characterTransitionClass`, `referenceTransitionClass`); `Main.keyboardDecoder`; `Main.splitterDecoders` / `panDecoder` / `stopPanDecoder`; `Main.nodeMoveDecoder` / `nodeMoveDecoders`; `GraphEditor.nodeClickDecoder` / `nodeDragStartDecoder` / `linkClickMsg`.
- **Every item in `UI-Tests.md` is ticked** — the original checklist, the event-decoder and geometry sections, and the Batch 17–20 sections.
