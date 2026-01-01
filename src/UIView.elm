module UIView exposing
    ( viewInteractionsDictSummary
    , viewConnectionEditor
    , viewMainInterface
    , viewNodeSplitInterface
    , viewPackageDeletionWarning
    )
import AutoDict
import Automata.Data exposing (..)
import Automata.Debugging exposing (debugAutomatonGraph, debugAutomatonGraphXY, debugGraph, debugLog_, println)
import Automata.DFA as DFA
import AutoSet
import Basics.Extra exposing (..)
import Browser
import Browser.Events as BE
import Changes as C
import Css exposing (px)
import Dict exposing (Dict)
import Force
import Graph exposing (NodeId)
import GraphEditor
import Html.Styled exposing (button, div, h1, Html, input, li, p, span, text, textarea, toUnstyled, ul)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import IntDict
import Json.Decode as D
import Json.Encode as E
import Jsonify exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import Ports exposing (..)
import Queries as Q
import Random.Pcg.Extended as Random
import Set
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Task
import Time
import TypedSvg exposing (g)
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Types exposing (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..) , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import UILogic as UI
import Uuid exposing (Uuid)

debugViewDimensions : Bool
debugViewDimensions = True

debugElement : String -> String -> Html a
debugElement otherClass s =
  if debugViewDimensions then
    div
      [ HA.class "debug dimensions-parent" ]
      [ div
        [ HA.class <| "dimensions " ++ otherClass ]
        [ text s ]
      ]
  else
    div
      []
      []

debugDimensions : Dimension -> Html a
debugDimensions {w, h} =
  debugElement "width height" (String.fromFloat w ++ "×" ++ String.fromFloat h)

debugHeight : Float -> Html a
debugHeight h =
  debugElement "height" (String.fromFloat h)

debugWidth : Float -> Html a
debugWidth w =
  debugElement "width" (String.fromFloat w)

viewNavigatorsArea : Model -> Html Msg
viewNavigatorsArea model =
  div
    [ HA.class "sidebar-container" ]
    [ if not model.uiLayout.open.sideBar then
        div [] []
      else
        div
          [ HA.class "navigation-bar" ]
          [ button
              [ HA.classList
                  [ ("navigation-icon computation-icon", True)
                  , ("active", model.uiLayout.selected.sideBar == ComputationsIcon)
                  ]
              , HA.title "Computations"
              , (model.uiLayout.selected.sideBar /= ComputationsIcon)
                |> thenPermitInteraction (HE.onClick (SelectNavigation ComputationsIcon))
              ]
              [ text "📁"]
          , button
              [ HA.classList
                  [ ("navigation-icon test-icon", True)
                  , ("active", model.uiLayout.selected.sideBar == TestsIcon)
                  ]
              , HA.title "Tests"
              , (model.uiLayout.selected.sideBar /= TestsIcon)
                |> thenPermitInteraction (HE.onClick (SelectNavigation TestsIcon))
              ]
              [ text "🧪"
              , AutoDict.get model.selectedPackage model.packages
                |> Maybe.map
                  (\pkg ->
                    let
                      (pass, fail, error) =
                        AutoDict.toList pkg.tests
                        |> List.foldl
                          (\(_, test) (p, f, e) ->
                            case isPassingTest test of
                              Just True ->
                                (p + 1, f, e)
                              Just False ->
                                (p, f + 1, e)
                              Nothing ->
                                (p, f, e + 1)
                          )
                          (0, 0, 0)
                      (testClass, number, testTitle) =
                        case (pass, fail, error) of
                          (0, 0, 0) ->
                            ( "nothing" -- --dracula-foreground
                            , "…"
                            , "No tests exist"
                            )
                          (_, 0, 0) -> -- no failures, no errors, and only passes.
                            ( "all-pass" -- --dracula-green
                            , "💯"
                            , "All tests passed!"
                            )
                          (0, _, 0) -> -- no passes, no errors, only failures.
                            ( "all-fail" -- --dracula-red
                            , "😵‍💫"
                            , "All tests failed!"
                            )
                          (_, _, 0) -> -- some passes, some failures, no errors
                            ( "some-fail" -- --dracula-pink
                            , String.fromInt fail
                            , if fail == 1 then
                                "1 test failed"
                              else
                                String.fromInt fail ++ " tests failed"
                            )
                          _ -> -- internal error exists!
                            ( "internal-error" -- --dracula-yellow
                            , "❓"
                            , "Some tests did not complete due to an internal error! This indicates a problem with the computation system. Please report it to the developer."
                            )
                    in
                      div
                        [ HA.class ("quick-status " ++ testClass)
                        , HA.title testTitle
                        ]
                        [ text number ]
                  )
                |> Maybe.withDefault (text "")
              ]
          ]
    , if not model.uiLayout.open.sideBar then
        text ""
      else
        div
          [ HA.class "sidebar"
          , HA.css
              [ Css.width <| Css.px <|
                  if model.uiLayout.open.sideBar then
                    model.uiLayout.dimensions.sideBar.w
                  else
                    0
              ]
          ]
          [ debugDimensions model.uiLayout.dimensions.sideBar
          , case model.uiLayout.selected.sideBar of
              ComputationsIcon ->
                viewComputationsSidebar model
              TestsIcon ->
                AutoDict.get model.selectedPackage model.packages
                |> Maybe.map (flip viewTestsSidebar model)
                |> Maybe.withDefault
                  (div [ HA.class "error graph-not-loaded" ] [ text "⚠ No graph is loaded in the editor" ])
          ]
    ]

viewTestsSidebar : GraphPackage -> Model -> Html Msg
viewTestsSidebar pkg {properties} =
  let
    (expectAccept, expectReject) =
      AutoDict.toList pkg.tests
      |> List.partition (Tuple.second >> .expectation >> (==) ExpectAccepted)
    displayTests headingHtml tx =
      case tx of
        [] ->
          text ""
        _ ->
          div
            []
            [ headingHtml
            , ul
                [ HA.class "input-list" ]
                ( List.sortBy (Tuple.second >> .input) tx
                  |> List.map
                    (\(key, test) ->
                      li
                        [ HA.classList
                            [ ("test-input", True)
                            , ("selected", key == pkg.currentTestKey)
                            , ("disabled", not properties.canLoadTestInput)
                            ]
                        , HA.title "Edit"
                        ]
                        [ viewTestItemInPanel (key, test) ]
                    )
                )
            ]
    viewTestItemInPanel : (Uuid, Test) -> Html Msg
    viewTestItemInPanel (key, test) =
      let
        testStatus = isFailingTest test
      in
        div 
          [ HA.classList
              [ ("failing", testStatus == Just True)
              , ("passing", testStatus == Just False)
              , ("error", testStatus == Nothing)
              ]
          , properties.canLoadTestInput
            |> thenPermitInteraction (HE.onClick (PackageMsg pkg.packageIdentifier <| SelectTest key))
          ]
          [ span
              [ HA.classList
                  [ ("test-input-container", True)
                  , ("disabled", not properties.canLoadTestInput)
                  ]
              , case test.result of
                  InternalError s -> HA.title s
                  _ -> HA.hidden False -- … pretty irrelevant. Which is what I want.
              ]
              [ text test.input ]
          , if properties.canDeleteTestInput && not (key == pkg.currentTestKey) then
              div
                [ HA.classList
                    [ ("button", True)
                    , ("disabled", not properties.canDeleteTestInput)
                    ]
                , HA.title "Delete test"
                , HE.onClick (PackageMsg pkg.packageIdentifier <| DeleteTestInput key)
                ]
                [ text "🚮" ]
            else
              text ""
          ]
  in
    div
      [ HA.class "sidebar-content tests-explorer" ]
      [ h1
          [ HA.class "sidebar-title" ]
          [ text "Test Inputs "
          , if properties.canCreateTestInput then
              button
                [ HA.class "add-button"
                , HA.title "Add new test"
                , HE.onClick (PackageMsg pkg.packageIdentifier CreateNewTestInput)
                ]
                [ text "➕" ]
            else
              text ""
          ]
      , div
          [ HA.class "package-description" ]
          [ pkg.computation.description
            |> Maybe.map text
            |> Maybe.withDefault (text "")
          ]
      , div
          [ HA.class "test-inputs expect-accepted" ]
          [ displayTests
              ( div
                  [ HA.class "title" ]
                  [ span [] [ text "Should be " ]
                  , span [ HA.class "emphasis" ] [ text "accepted" ]
                  ]
              )
              expectAccept
          ]
      , div
          [ HA.class "test-inputs expect-rejected" ]
          [ displayTests
              ( div
                  [ HA.class "title" ]
                  [ span [] [ text "Should be " ]
                  , span [ HA.class "emphasis" ] [ text "rejected" ]
                  ]
              )
              expectReject
          ]




    ]

viewComputationsSidebar : Model -> Html Msg
viewComputationsSidebar model =
  div
    [ HA.class "sidebar-content computations" ]
    [ h1
        [ HA.class "sidebar-title" ]
        [ text "Computations "
        , if model.properties.canCreateNewPackage then
            button
              [ HA.class "add-button"
              , HA.title "Create new computation"
              , HE.onClick CreateNewPackage
              ]
              [ text "➕" ]
          else
            text ""
        ]
    , let
        mainPkgUuid =
          AutoDict.get model.mainGraphView model.graph_views
          |> Maybe.andThen .graphPackage
      in
      div
        [ HA.class "computations-explorer" ]
        ( List.filterMap
            (\gv_uuid ->
                AutoDict.get gv_uuid model.graph_views
                |> Maybe.andThen
                  (\graph_view ->
                    Maybe.map
                      (\pkg_uuid ->
                        div
                          [ HA.class "package"
                          , graph_view.properties.canSelectPackage
                            |> thenPermitInteraction (HE.onClick (PackageMsg pkg_uuid SelectPackage))
                          ]
                          [ GraphEditor.viewGraph graph_view
                          , div
                              [ HA.class "description" ]
                              [ graph_view.computation.description
                                |> Maybe.withDefault "(no description)"
                                |> text
                              ]
                          , if graph_view.graphPackage == mainPkgUuid || not graph_view.properties.canDeletePackage then
                              text ""
                            else
                              div
                                [ HA.class "delete-button"
                                , HA.title "Delete"
                                , HE.onClick (PackageMsg pkg_uuid DeletePackage)
                                ]
                                [ text "🚮" ]
                          ]
                      )
                      graph_view.graphPackage
                  )
            )
            model.computationsExplorer
        )
    ]

viewCollapsedAreaButton : SplitterMovement -> Html Msg
viewCollapsedAreaButton movement =
  let
    (movementClass, targetArea) =
      case movement of
        LeftRight ->
          ( "leftright", NavigatorsArea )
        UpDown ->
          ( "updown", ToolsArea )
    collapseIcon =
      svg
        [ Svg.Styled.Attributes.class ("collapse-icon " ++ movementClass)
        , Svg.Styled.Attributes.viewBox "4 4 10 8"
        ]
        [ Svg.Styled.path
            [ Svg.Styled.Attributes.d "M10 12L6 8l4-4" ]
            []
        ]
  in
    button
      [ HA.class <| "collapse-button " ++ movementClass ++ " collapsed"
      , HA.title "Expand"
      , HE.onClick (ToggleAreaVisibility targetArea)
      ]
      [ collapseIcon ]

-- unconditionally display a splitter
viewSplitter : Int -> SplitterMovement -> Model -> Bool -> Html Msg
viewSplitter zIdx movement model areaOpen =
  let
    (movementClass, targetArea) =
      case movement of
        LeftRight ->
          ( "leftright", NavigatorsArea )
        UpDown ->
          ( "updown", ToolsArea )
    collapseIcon =
      svg
        [ Svg.Styled.Attributes.class ("collapse-icon " ++ movementClass)
        , Svg.Styled.Attributes.viewBox "4 4 10 8"
        ]
        [ Svg.Styled.path
            [ Svg.Styled.Attributes.d "M10 12L6 8l4-4" ]
            []
        ]
    topOfStack =
      Q.peekInteraction Nothing model.interactionsDict
  in
    div
      [ HA.classList
          [ ("splitter-separator " ++ movementClass, True)
          , ("dragging", topOfStack == Just (DraggingSplitter movement))
          , ( "draggable", model.properties.canDragSplitter && areaOpen)
          ]
      , HA.css
          [ Css.zIndex (Css.int zIdx)
          ]
      , HE.onMouseDown (StartDraggingSplitter movement)
      ]
      [ div
          [ HA.class <| "separator-handle " ++ movementClass ]
          [ button
              [ HA.class <| "collapse-button " ++ movementClass
              , HA.title "Collapse"
              , HE.onClick (ToggleAreaVisibility targetArea)
              ]
              [ collapseIcon ]
          ]
      ]

viewTestingTool : GraphPackage -> Test -> Model -> Html Msg
viewTestingTool pkg test model =
  let
    html_remaining_data =
      List.map (\ch ->
        div
          [ HA.class "character" ]
          [ text <| String.fromChar ch ]
      )
    html_transitions_taken =
      List.map (\{matching} ->
        div
          [ HA.class "transition" ]
          [ text <| acceptConditionToString matching.via ]
      )
    contextChars = 3
    html_summary_step h =
      div
        [ HA.class "execution-step-inner" ]
        [ div
            [ HA.class "summary" ]
            [ if List.isEmpty h.transitions then
                -- If there are no transitions, then the div will collapse weirdly
                -- and that will throw off the calculations for the vertical position
                -- of the list-item bullet, and of course it will, because that is
                -- the obvious thing to happen, isn't it?
                -- Wow. CSS, eh? CSS.
                -- So, this exists to counteract that 100% obvious flex-behaviour.
                text ""
              else
                div
                  [ HA.class "transitions-taken" ]
                  ((if List.length h.transitions > contextChars then
                      div
                        [ HA.class "ellipsis" ]
                        [ text "…" ]
                    else
                      text ""
                  ) ::
                  ( html_transitions_taken <| List.drop (List.length h.transitions - contextChars) h.transitions )
                  )
            , if List.isEmpty h.remainingData then
                text ""
              else
                div
                  [ HA.class "remaining-data" ]
                  ((html_remaining_data <| List.take contextChars h.remainingData) ++
                    ( if List.length h.remainingData > contextChars then
                        [ div
                            [ HA.class "ellipsis" ]
                            [ text "…" ]
                        ]
                      else
                        []
                    )
                  )
            ]
        ]
    html_expanded_step gv h =
      div
        [ HA.class "execution-step-inner" ]
        [ div
            [ HA.class "summary" ]
            [ case h.transitions of
                [] ->
                  text ""
                _ ->
                  div
                    [ HA.class "transitions-taken" ]
                    ( html_transitions_taken h.transitions )              
            , case h.remainingData of
                [] ->
                  text ""
                _ ->
                  div
                    [ HA.class "remaining-data" ]
                    (html_remaining_data h.remainingData)
            ]
        , div
            [ HA.class "step-graph" ]
            [ GraphEditor.viewGraph gv ]
        ]
    html_execution_step props h =
      li
        [ HA.class "execution-step"
        , HE.onClick (ToggleDebugStep h.step)
        ]
        [ case IntDict.get h.step props.expandedSteps of
            Just gv_uuid ->
              AutoDict.get gv_uuid model.graph_views
              |> Maybe.map (flip html_expanded_step h)
              |> Maybe.withDefaultLazy (\() -> html_summary_step h)
            Nothing ->
              html_summary_step h
        ]
    html_execution_steps props results =
      ul
        [ HA.class "execution-steps" ]
        ( List.map (html_execution_step props) results )
  in
  div
    [ HA.class "tool-content testing" ]
    [ case Q.peekInteraction Nothing model.interactionsDict of
        Just (Executing ((h::t) as results) props) ->
          div
            [ HA.class "step-through" ]
            ( case h.finalResult of
                Just result ->
                  -- final; show it.
                  [ div
                      [ HA.class "tools-strip" ]
                      [ button
                          [ HA.class "tool-icon reset"
                          , HA.title "Reset"
                          , HE.onClick ResetComputation
                          ]
                          [ text "🔁" ]
                      , button
                          [ HA.class "tool-icon step-back"
                          , HA.title "Step back"
                          , HE.onClick StepBack
                          ]
                          [ text "↩️" ]
                      ]
                  , case result of
                      Accepted ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result accepted" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Accepted" ]
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      Rejected ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result rejected" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Rejected" ]
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      NoMatchingTransitions ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result rejected" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Failed" ]
                                  , text ".  The computation did not account for "
                                  , span [ HA.class "emphasis" ] [ text "all" ]
                                  , text " of the input."
                                  ]
                              , div
                                  [ HA.class "summary-detail" ]
                                  [ text "Remaining input: "
                                  , div
                                      [ HA.class "remaining-data" ]
                                      ( html_remaining_data h.remainingData )
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      InternalError s ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result error" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ text <| "💀 Internal Error!  " ++ s ]
                              ]
                          , html_execution_steps props results
                          ]
                  ]
                Nothing ->
                  -- non-final; we're continuing.
                  [ div
                      [ HA.class "tools-strip" ]
                      [ button
                          [ HA.class "tool-icon continue-stepping"
                          , HA.title "Continue"
                          , HE.onClick Step
                          ]
                          [ text "⏯️" ]
                      , button
                          [ HA.class "tool-icon stop-stepping"
                          , HA.title "Run to end"
                          , HE.onClick Run
                          ]
                          [ text "▶️" ]
                      , button
                          [ HA.class "tool-icon stop-stepping"
                          , HA.title "Stop / Reset"
                          , HE.onClick ResetComputation
                          ]
                          [ text "⏹️" ]
                      , button
                          [ HA.class "tool-icon step-back"
                          , HA.title "Step back"
                          , HE.onClick StepBack
                          ]
                          [ text "↩️" ]
                      ]
                  , div
                      [ HA.class "progress-area" ]
                      [ html_execution_steps props results ]
                  ]
            )
        _ ->
          -- edit panel.  We are not executing.
          div
            [ HA.class "edit-input" ]
            [ div
                [ HA.class "tools-strip" ]
                [ button
                    [ HA.class "tool-icon start-stepping"
                    , HA.title "Step through"
                    , HE.onClick Step
                    ]
                    [ text "⏯️" ]
                , button
                    [ HA.class "tool-icon run"
                    , HA.title "Run"
                    , HE.onClick Run
                    ]
                    [ text "▶️" ]
                ]
            , div
                [ HA.class "edit-panel" ]
                [ div
                    [ HA.class "acceptance-condition" ]
                    [ text "When this input is received, the computation should "
                    , span
                        [ HA.classList
                            [ ("expect-accept", test.expectation == ExpectAccepted)
                            , ("expect-reject", test.expectation == ExpectRejected)
                            ]
                        , HE.onClick (PackageMsg pkg.packageIdentifier FlipAcceptanceCondition)
                        ]
                        [ text
                            ( case test.expectation of
                                ExpectAccepted -> "accept"
                                ExpectRejected -> "reject"
                            )
                        ]
                    , text " it."
                    ]
                , textarea
                    [ HA.class "input-textarea"
                    , HA.value test.input
                    , HE.onInput (UpdateTestInput >> PackageMsg pkg.packageIdentifier)
                    , HA.placeholder "Enter your test input here"
                    ]
                    []
                ]
            ]
    ]

viewMetadataTool : GraphPackage -> Html Msg
viewMetadataTool package =
  let
    len = package.computation.description |> Maybe.withDefault "" |> String.length |> toFloat
    idx = round <| -0.02779853 + 0.06685532 * len + 0.004036796 * len * len
    color = List.getAt idx colorScale.css.best_to_worst |> Maybe.withDefault colorScale.css.worst
  in
  div
    [ HA.class "tool-content metadata" ]
    [ div
        [ HA.class "edit-description" ]
        [ div
            [ HA.class "edit-panel" ]
            [ textarea
                [ HA.class "input-textarea"
                , HA.css [ Css.color color ]
                , HA.value (Maybe.withDefault "" package.computation.description)
                , HE.onInput (UpdateComputationDescription >> PackageMsg package.packageIdentifier)
                , HA.placeholder "What kind of thing does this package recognize?"
                ]
                []
            ]
        ]
    ]

viewToolsArea : Model -> Html Msg
viewToolsArea model =
  div
    [ HA.class "tools-container" ]
    [ div
        [ HA.class "tools-bar" ]
        [ button
            [ HA.classList
                [ ("tool-icon", True)
                , ("active", model.uiLayout.selected.bottomPanel == TestingToolIcon)
                ]
            , HA.title "Testing"
            , (model.uiLayout.selected.bottomPanel /= TestingToolIcon)
              |> thenPermitInteraction (HE.onClick (SelectTool TestingToolIcon))
            ]
            [ text "🔬"]
        , button
            [ HA.classList
                [ ("tool-icon", True)
                , ("active", model.uiLayout.selected.bottomPanel == MetadataToolIcon)
                ]
            , HA.title "Tests"
            , (model.uiLayout.selected.bottomPanel /= MetadataToolIcon)
              |> thenPermitInteraction (HE.onClick (SelectTool MetadataToolIcon))
            ]
            [ text "📝" ]
        ]
    , div
        [ HA.class "tools-area"
        , HA.css
            [ Css.height <| Css.px <|
                if model.uiLayout.open.bottomPanel then
                  model.uiLayout.dimensions.bottomPanel.h
                else
                  0
            ]
        ]
        [ debugDimensions model.uiLayout.dimensions.bottomPanel
        , case model.uiLayout.selected.bottomPanel of
            MetadataToolIcon ->
              AutoDict.get model.selectedPackage model.packages
              |> Maybe.map (viewMetadataTool)
              |> Maybe.withDefault
                ( div
                    [ HA.class "metadata-content no-package" ]
                    [ text "Internal error: no graph-view.  This is a bug!" ]
                )
            TestingToolIcon ->
              AutoDict.get model.selectedPackage model.packages
              |> Maybe.map (\pkg ->
                ( pkg
                , AutoDict.get pkg.currentTestKey pkg.tests
                  |> Maybe.withDefault
                    { input = ""
                    , expectation = ExpectAccepted
                    , result = Rejected
                    }
                )
              )
              |> Maybe.map (\(pkg, test) ->
                viewTestingTool pkg test model
              )
              |> Maybe.withDefault
                ( div
                    [ HA.class "tool-content no-tests" ]
                    [ text "There are no tests for this computation." ]
                )
        ]
    ]

viewMainInterface : Model -> Html Msg
viewMainInterface model =
  div
    [ HA.class "editor-frame" ]
    [ viewNavigatorsArea model
    , if model.uiLayout.open.sideBar then
        viewSplitter
          5 LeftRight
          model
          (model.uiLayout.open.sideBar)
      else
        div [] []
    , div
        [ HA.class "editor-and-tools-panel" ]
        [ div
            [ HA.class "editor-main"
            , HA.css
                [ Css.maxWidth <| px <| model.uiLayout.dimensions.mainEditor.w
                , Css.maxHeight <| px <| model.uiLayout.dimensions.mainEditor.h
                ]
            ]
            [ debugDimensions model.uiLayout.dimensions.mainEditor
            , AutoDict.get model.mainGraphView model.graph_views
              |> Maybe.map (GraphEditor.viewGraph)
              |> Maybe.withDefault
                (div [ HA.class "error graph-not-loaded" ] [ text "⚠ Graph to load was not found!" ]) -- erk! say what now?!
            ]
        , viewSplitter
            4 UpDown
            model
            model.uiLayout.open.bottomPanel
        , viewToolsArea model
        ]
    -- we have these down here so that they WILL sit on top of the panning-region bars for the
    -- main area.
    , if not model.uiLayout.open.sideBar then
        viewCollapsedAreaButton LeftRight
      else
        text ""
    , if not model.uiLayout.open.bottomPanel then
        viewCollapsedAreaButton UpDown
      else
        text ""
    ]

htmlForTransition : Model -> Transition -> Maybe (Html Msg)
htmlForTransition {packages} {via, isFinal} =
  let
    classes =
      [ ("set-item", True)
      , ("final", isFinal)
      , ("non-final", not isFinal)
      ]
  in
    case via of
      ViaCharacter c ->
        Just
          ( span
              [ HA.classList ( ("character", True) :: classes )
              ]
              [ text <| String.fromChar c ]
          )
      ViaGraphReference pkg_uuid ->
        AutoDict.get pkg_uuid packages
        |> Maybe.map (\pkg ->
          span
            [ HA.classList ( ("graph-reference", True) :: classes ) ]
            [ div
                [ HA.class "package-badge"
                , HA.title "This unique badge identifies this specific computation."
                ]
                [ TypedSvg.svg
                    [ TypedSvg.Attributes.viewBox 0 0 30 18 ]
                    [ GraphEditor.viewGraphReference pkg_uuid 4 0 0 ]
                  |> Html.Styled.fromUnstyled
                ]
            , case pkg.computation.description of
                Just s ->
                  div
                    [ HA.class "description" ]
                    [ text s ]
                Nothing ->
                  text ""
            ]
            -- [ text <| Maybe.withDefault "(no description)" pkg.description ]
        )


viewConnectionEditor : Model -> Connection -> ConnectionEditorProperties -> Html Msg
viewConnectionEditor model connection editorData =
  let
    (terminal, nonTerminal) =
      AutoSet.partition (.isFinal) connection
  in
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "connection-editor" ]
        [ div
            [ HA.class "connection-editor-top" ]
            [ div
                [ HA.class "top-left" ]
                [ div
                    [ HA.class "set-builder" ]
                    [ div -- set visualization area
                        [ HA.class "set-visualization" ]
                        [ div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- terminal items group
                            [ HA.class "terminals" ]
                            ( AutoSet.toList terminal
                              |> List.filterMap (htmlForTransition model)
                            )
                        , div
                            [ HA.class "set-separator" ]
                            []
                        , div -- normal (non-terminal) items group
                            [ HA.class "non-terminals" ]
                            ( AutoSet.toList nonTerminal
                              |> List.filterMap (htmlForTransition model)
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        ]
                    ]
                , div -- quick input
                    [ HA.class "quick-input" ]
                    [ div
                        [ HA.class "quick-input-bar"
                        , HE.onInput (QuickInput)
                        ]
                        [ input
                            [ HA.class "input-field"
                            , HA.placeholder "Type here…"
                            , HA.id "quick-input"
                            , HA.autocomplete False
                            , HA.attribute "autocorrect" "off"
                            , HA.attribute "spellcheck" "off"
                            , HA.value <|
                                case editorData.editingMode of
                                  CharacterInput -> ""
                                  GraphReferenceSearch s -> s
                            ]
                            []
                        -- , button
                        --     [ HA.class "action-button" ]
                        --     [ span [] [ text "🎨 Images" ] ]
                        -- , button
                        --     [ HA.class "action-button secondary" ]
                        --     [ span [] [ text "🔍 Browse All" ] ]
                        ]
                    , div
                        [ HA.class "instructions" ]
                        ( case editorData.editingMode of
                            CharacterInput ->
                              [ div
                                  []
                                  [ span [] [ text "Typing a character once adds it; typing it again promotes it to " ]
                                  , span [ HA.class "terminal-style" ] [ text "terminal" ]
                                  , span [] [ text " status; typing it a third time removes it.  Use the mouse to scroll and select computations." ]
                                  ]
                              , div
                                  []
                                  [ span [] [ text "Typing «`» enters computation-search mode." ]
                                  ]
                              ]
                            GraphReferenceSearch _ ->
                              [ div
                                  []
                                  [ span [] [ text "Only computations with a matching description will be displayed.  Use the mouse to scroll and select computations.  Type «`» to switch back to character mode." ]
                                  ]
                              ]
                        )
                    ]
                ]
            , div
                [ HA.class "top-right" ]
                [ AutoDict.get editorData.mainGraph model.graph_views
                  |> Maybe.map GraphEditor.viewGraph
                  |> Maybe.withDefault (text "")
                ]
            ]
        , div -- image palette
            [ HA.class "image-palette" ]
            [ div
                [ HA.class "palette-grid" ]
                -- good width for a 4x? grid: 450-455px
                ( List.filterMap
                    (\view_uuid ->
                        AutoDict.get view_uuid model.graph_views
                        |> Maybe.andThen
                          (\gv ->
                            Maybe.combineSecond
                              ( gv
                              , Maybe.andThen
                                  (\pkg_uuid -> AutoDict.get pkg_uuid model.packages)
                                  gv.graphPackage
                              )
                          )
                        |> Maybe.map
                          (\(graph_view, pkg) ->
                            let
                              via = ViaGraphReference pkg.packageIdentifier
                              inTerminals = AutoSet.member (Transition True via) terminal
                              inNonTerminals = AutoSet.member (Transition False via) nonTerminal
                            in
                              -- case graph_view. graph_view.package.description of
                              --   Nothing -> -- then display anyway.
                              --   Just k ->
                              div
                                [ HA.classList
                                    [ ("palette-item", True)
                                    , ("terminal", inTerminals)
                                    , ("non-terminal", inNonTerminals)
                                    ]
                                , HE.onClick (ToggleConnectionTransition via)
                                ]
                                [ GraphEditor.viewGraph graph_view
                                , div
                                    [ HA.class "description" ]
                                    [ graph_view.computation.description
                                      |> Maybe.withDefault "(no description)"
                                      |> text
                                    ]
                                , if inTerminals || inNonTerminals then
                                    div
                                      [ HA.class "package-badge"
                                      , HA.title "This unique badge identifies this specific computation."
                                      ]
                                      [ TypedSvg.svg
                                          [ TypedSvg.Attributes.viewBox 0 0 30 18 ]
                                          [ GraphEditor.viewGraphReference pkg.packageIdentifier 4 0 0 ]
                                        |> Html.Styled.fromUnstyled
                                      ]
                                  else
                                    text ""
                                ]
                        )
                    )
                    editorData.shownList
                )
            , div
                []
                []
            ]
        ]
    ]

viewNodeSplitInterface : Model -> NodeSplitData -> SplitNodeInterfaceProperties -> Html Msg
viewNodeSplitInterface model {left, right} interfaceData =
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "node-split-interface" ]
        [ div
            [ HA.class "node-split-top" ]
            [ div
                [ HA.class "top-left" ]
                [ div
                    [ HA.class "set-builder" ]
                    [ div -- set visualization area
                        [ HA.class "set-visualization" ]
                        [ div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- terminal items group
                            [ HA.class "left" ]
                            ( AutoSet.toList left
                              |> List.filterMap (\t -> Maybe.combineSecond (t, htmlForTransition model t))
                              |> List.map (\({via}, e) -> div [ HE.onClick (ToggleConnectionTransition via) ] [ e ])
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        , div
                            [ HA.class "set-separator" ]
                            []
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- normal (non-terminal) items group
                            [ HA.class "right" ]
                            ( AutoSet.toList right
                              |> List.filterMap (\t -> Maybe.combineSecond (t, htmlForTransition model t))
                              |> List.map (\({via}, e) -> div [ HE.onClick (ToggleConnectionTransition via) ] [ e ])
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        ]
                    ]
                , div -- quick input
                    [ HA.class "quick-input" ]
                    [ div
                        [ HA.class "quick-input-bar"
                        , HE.onInput (QuickInput)
                        ]
                        [ input
                            [ HA.class "input-field"
                            , HA.placeholder "Type here…"
                            , HA.id "quick-input"
                            , HA.autocomplete False
                            , HA.attribute "autocorrect" "off"
                            , HA.attribute "spellcheck" "off"
                            , HA.value ""
                            ]
                            []
                        ]
                    , div
                        [ HA.class "instructions" ]
                            [ div
                                []
                                [ span [] [ text "Typing a character will switch it from right to left, or vice-versa.  You can also click on items above, including computations, to switch their sides." ]
                                ]
                            ]
                    ]
                ]
            , div
                [ HA.class "top-right" ]
                [ AutoDict.get interfaceData.mainGraph model.graph_views
                  |> Maybe.map GraphEditor.viewGraph
                  |> Maybe.withDefault (text "")
                ]
            ]
        ]
    ]

viewPackageDeletionWarning : DeletingPackageProperties -> Model -> Html Msg
viewPackageDeletionWarning props model =
  let
    viewGraphItem uuid =
      AutoDict.get uuid model.graph_views
      |> Maybe.map
        (\graph_view ->
          div
            [ HA.class "graph-item" ]
            [ GraphEditor.viewGraph graph_view
            , div
                [ HA.class "description" ]
                [ graph_view.computation.description
                  |> Maybe.withDefault "(no description)"
                  |> text
                ]
            ]
        )
  in
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "deletion-warning" ]
        [ div
            [ HA.class "title" ]
            [ text "Are you sure you want to delete this?"
            ]
        , div
            [ HA.class "graph-to-delete" ]
            [ viewGraphItem props.mainGraph
              |> Maybe.withDefault (text "")
            ]
        , div
            [ HA.class "details" ]
            [ div
              [ HA.class "panel left" ]
              [ div
                  [ HA.class "info" ]
                  [ text "Deletion will directly affect" ]
              , div
                  [ HA.class "affected" ]
                  ( List.filterMap viewGraphItem props.directViews )
              ]
            , div
              [ HA.class "panel right" ]
              [ div
                  [ HA.class "info" ]
                  [ text "Deletion will indirectly affect" ]
              , div
                  [ HA.class "affected" ]
                  ( List.filterMap viewGraphItem props.indirectViews )
              ]
            ]
        , div
            [ HA.class "buttons" ]
            [ button
                [ HE.onClick Confirm
                , HA.class "button danger"
                ]
                [ text "Delete this graph"
                ]
            , button
                [ HE.onClick Escape
                , HA.class "button"
                ]
                [ text "Keep this graph"
                ]
            ]
        ]
    ]

viewInteractionsDictSummary : AutoDict.Dict String (Maybe Uuid) (Int, List InteractionState) -> Html a
viewInteractionsDictSummary dict =
  AutoDict.toList dict
  |> List.sortBy (Tuple.second >> Tuple.first)
  |> List.map (\(key, (_, stack)) ->
    let
      k =
        case key of
          Nothing -> "⯁"
          Just uuid -> truncate_uuid uuid
    in
        k ++ " → " ++ String.fromInt (List.length stack)
  )
  |>
    (\strings ->
      Html.Styled.ol
        []
        ( List.map (\s -> Html.Styled.li [] [ text s ]) strings )
    )