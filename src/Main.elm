module Main exposing (..)

import Browser
import Browser.Events as BE
import Html.Styled exposing (Html, div, h3, p, ul, li, input, textarea, span, toUnstyled, text, button, strong)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph as FDG
import Automata.Data exposing (..)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Html.Styled.Attributes as HA
import List.Extra
import Automata.DFA
import Html
import Maybe.Extra
import Css
import Platform.Cmd as Cmd

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type LeftPanelIcon
  = FileIcon
  | SearchIcon
  | GitIcon
  | TestsIcon
  | ExtensionsIcon

type ExecutionState
  = Ready
  | NotReady
  | ExecutionComplete
  | StepThrough

type BottomPanel
  = AddTestPanel
  | EditDescriptionPanel

type alias Model =
  { forceDirectedGraph : FDG.Model
  , mainPanelDimensions : ( Float, Float )
  , leftPanelOpen : Bool
  , selectedIcon : Maybe LeftPanelIcon
  , leftPanelWidth : Float
  , rightBottomPanelOpen : Bool
  , rightBottomPanelHeight : Float
  , rightTopPanelDimensions : ( Float, Float )
  , testPanelContent : String
  , isDraggingHorizontalSplitter : Bool
  , isDraggingVerticalSplitter : Bool
  , mousePosition : ( Float, Float )
  , executionState : ExecutionState
  , selectedBottomPanel : BottomPanel
  , descriptionPanelContent : String
  }

decodeDimensions : D.Decoder ( Float, Float )
decodeDimensions =
  D.map2
    (\w h -> ( w, h ))
    (D.field "width" D.float)
    (D.field "height" D.float)

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    (width, height) =
      D.decodeValue decodeDimensions flags
      |> Result.withDefault ( 800, 600 )
    
    initialRightTopWidth = width - 60  -- 60px for icon bar
    initialRightTopHeight = height - 223  -- 30px status bar + 185px bottom panel + 8px splitter
  in
    ( { forceDirectedGraph = FDG.receiveWords [] ( initialRightTopWidth, initialRightTopHeight )
      , mainPanelDimensions = ( width, height )
      , leftPanelOpen = False
      , selectedIcon = Nothing
      , leftPanelWidth = 250
      , rightBottomPanelOpen = True
      , rightBottomPanelHeight = 185
      , rightTopPanelDimensions = ( initialRightTopWidth, initialRightTopHeight )
      , testPanelContent = "" -- "// Welcome to the automaton editor\n// Type your code here..."
      , isDraggingHorizontalSplitter = False
      , isDraggingVerticalSplitter = False
      , mousePosition = ( 0, 0 )
      , executionState = Ready
      , selectedBottomPanel = AddTestPanel
      , descriptionPanelContent = ""
      }
    , Task.perform (\_ -> NoOp) (Task.succeed ())
    )

-- UPDATE

type Msg
  = ForceDirectedMsg FDG.Msg
  | OnResize (Float, Float)
  | SetMouseOver Bool
  | ClickIcon LeftPanelIcon
  | StartDraggingHorizontalSplitter
  | StartDraggingVerticalSplitter
  | StopDragging
  | MouseMove Float Float
  | ToggleBottomPanel
  | UpdateTestPanelContent String
  | UpdateDescriptionPanelContent String
  | UpdateRightTopDimensions Float Float
  | RunExecution
  | ResetExecution
  | StepThroughExecution
  | SaveTest
  | SelectBottomPanel BottomPanel
  | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ForceDirectedMsg fdMsg ->
      let
        newFdModel = FDG.update (0, 0) fdMsg model.forceDirectedGraph
      in
      ( { model
          | forceDirectedGraph = newFdModel
          , executionState =
              if FDG.canExecute newFdModel then
                if model.executionState == NotReady then
                  Ready
                else
                  model.executionState
              else
                NotReady
        }
      , Cmd.none
      )

    OnResize (width, height) ->
      let
        newModel = { model | mainPanelDimensions = (width, height) }
        (newRightTopWidth, newRightTopHeight, newGraph) = calculateRightTopDimensions newModel
      in
      ( { newModel
          | rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , forceDirectedGraph = newGraph
        }
      , Cmd.none
      )

    SetMouseOver _ ->
      ( model, Cmd.none )

    ClickIcon icon ->
      let
        (newLeftPanelOpen, newSelectedIcon) =
          if model.selectedIcon == Just icon && model.leftPanelOpen then
            (False, Nothing)  -- Close panel if same icon clicked
          else
            (True, Just icon)  -- Open panel with new icon
        (newRightTopWidth, newRightTopHeight, newGraph) = calculateRightTopDimensions { model | leftPanelOpen = newLeftPanelOpen }
      in
      ( { model 
        | leftPanelOpen = newLeftPanelOpen
        , selectedIcon = newSelectedIcon
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        , forceDirectedGraph = newGraph
        }
      , Cmd.none
      )

    StartDraggingHorizontalSplitter ->
      ( { model | isDraggingHorizontalSplitter = True }, Cmd.none )

    StartDraggingVerticalSplitter ->
      ( { model | isDraggingVerticalSplitter = True }, Cmd.none )

    StopDragging ->
      ( { model 
        | isDraggingHorizontalSplitter = False
        , isDraggingVerticalSplitter = False
        }
      , Cmd.none
      )

    MouseMove x y ->
      let
        newModel = { model | mousePosition = (x, y) }
      in
      if model.isDraggingHorizontalSplitter then
        let
          (viewportWidth, _) = model.mainPanelDimensions
          minWidth = 100
          maxWidth = viewportWidth / 2
          newLeftPanelWidth = clamp minWidth maxWidth x
          (newRightTopWidth, newRightTopHeight, newGraph) = calculateRightTopDimensions { newModel | leftPanelWidth = newLeftPanelWidth }
        in
        ( { newModel 
          | leftPanelWidth = newLeftPanelWidth
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , forceDirectedGraph = newGraph
          }
        , Cmd.none
        )
      else if model.isDraggingVerticalSplitter then
        let
          (_, viewportHeight) = model.mainPanelDimensions
          minHeight = 185  -- 8em @ 16px font ≈ 128px
          maxHeight = viewportHeight / 2
          statusBarHeight = 30
          newBottomHeight = clamp minHeight maxHeight (viewportHeight - y - statusBarHeight)
          (newRightTopWidth, newRightTopHeight, newGraph) = calculateRightTopDimensions { newModel | rightBottomPanelHeight = newBottomHeight }
        in
        ( { newModel 
          | rightBottomPanelHeight = newBottomHeight
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , forceDirectedGraph = newGraph
          }
        , Cmd.none
        )
      else
        ( newModel, Cmd.none )

    ToggleBottomPanel ->
      let
        newBottomPanelOpen = not model.rightBottomPanelOpen
        (newRightTopWidth, newRightTopHeight, newGraph) = calculateRightTopDimensions { model | rightBottomPanelOpen = newBottomPanelOpen }
      in
      ( { model 
        | rightBottomPanelOpen = newBottomPanelOpen
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        , forceDirectedGraph = newGraph
        }
      , Cmd.none
      )

    UpdateTestPanelContent content ->
      ( { model | testPanelContent = content }, Cmd.none )

    UpdateDescriptionPanelContent content ->
      ( { model | descriptionPanelContent = content }, Cmd.none )

    UpdateRightTopDimensions width height ->
      ( { model | rightTopPanelDimensions = (width, height) }, Cmd.none )

    RunExecution ->
      -- for this and StepThrough, if the FDG.canExecute is false, then
      -- the 'Run' and 'Step' buttons on the UI will be disabled.
      -- Therefore, I don't need to handle those cases here, and can
      -- assume that when I get this message, the precondition of
      -- a 'Ready' state has already been met.
      ( { model 
        | executionState = ExecutionComplete
        , forceDirectedGraph =
            fdg_update model (FDG.Load model.testPanelContent)
            |> \m -> fdg_update { model | forceDirectedGraph = m } FDG.Run
        }
      , Cmd.none
      )

    ResetExecution ->
      ( { model 
        | executionState = Ready
        , forceDirectedGraph = fdg_update model FDG.Stop
        }
      , Cmd.none
      )

    StepThroughExecution ->
      let
        newFdModel =
          if model.executionState /= StepThrough then
            -- this is the first click of stepping, so do a load first.
            fdg_update model (FDG.Load model.testPanelContent)
          else
            fdg_update model FDG.Step
      in
      ( { model 
        | executionState =
            case newFdModel.execution of
              Nothing ->
                model.executionState
              Just (CanContinue _) ->
                StepThrough
              _ ->
                ExecutionComplete
        , forceDirectedGraph =
            newFdModel
        }
      , Cmd.none
      )

    SelectBottomPanel p ->
      ( { model | selectedBottomPanel = p }
      , Cmd.none
      )

    SaveTest ->
      Debug.todo "SAVE TEST FUNCTIONALITY"

    NoOp ->
      ( model, Cmd.none )

fdg_update : Model -> FDG.Msg -> FDG.Model
fdg_update model updateMessage =
  FDG.update
    ( if model.leftPanelOpen then
        60 + model.leftPanelWidth
      else
        60
    , 1
    )
    updateMessage
    model.forceDirectedGraph


calculateRightTopDimensions : Model -> ( Float, Float, FDG.Model )
calculateRightTopDimensions model =
  let
    (viewportWidth, viewportHeight) = model.mainPanelDimensions
    iconBarWidth = 60
    leftPanelWidth = if model.leftPanelOpen then model.leftPanelWidth else 0
    statusBarHeight = 30
    bottomPanelHeight = if model.rightBottomPanelOpen then model.rightBottomPanelHeight else 0
    -- Always account for some splitter height (either full splitter or collapsed splitter)
    splitterHeight = if model.rightBottomPanelOpen then 8 else 4
    
    rightTopWidth = viewportWidth - iconBarWidth - leftPanelWidth
    rightTopHeight = viewportHeight - statusBarHeight - bottomPanelHeight - splitterHeight
    newGraph =
      fdg_update
        model
        (FDG.ViewportUpdated (rightTopWidth, rightTopHeight))
  in
  ( rightTopWidth, rightTopHeight, newGraph )

-- VIEW

view : Model -> Html Msg
view model =
  div 
    [ HA.class "main-container layout-flex-column" ]
    [ div 
      [ HA.class "main-content" ]
      [ viewLeftSection model
      , viewRightSection model
      ]
    , viewStatusBar model
    ]

viewLeftSection : Model -> Html Msg
viewLeftSection model =
  div 
    [ HA.class "left-section" ]
    [ viewIconBar model
    , if model.leftPanelOpen then
        div 
          [ HA.class "left-panel-container" ]
          [ viewLeftPanel model
          , viewHorizontalSplitter
          ]
      else
        text ""
    ]

viewIconBar : Model -> Html Msg
viewIconBar model =
  div 
    [ HA.class "icon-bar" ]
    [ viewIcon FileIcon "📁" model
    , viewIcon TestsIcon "🧪" model
    , viewIcon SearchIcon "🔍" model
    , viewIcon GitIcon "🌿" model
    , viewIcon ExtensionsIcon "🧩" model
    ]

viewIcon : LeftPanelIcon -> String -> Model -> Html Msg
viewIcon icon iconText model =
  let
    isSelected = model.selectedIcon == Just icon
    iconClass = if isSelected then "icon icon--selected" else "icon"
  in
  div 
    [ HA.class iconClass
    , onClick (ClickIcon icon)
    ]
    [ text iconText ]

viewLeftPanel : Model -> Html Msg
viewLeftPanel model =
  div 
    [ HA.class "left-panel"
    , HA.style "width" (String.fromFloat model.leftPanelWidth ++ "px")
    ]
    [ case model.selectedIcon of
        Just FileIcon ->
          div []
            [ h3 [] [ text "Graphs" ]
            -- , p [] [ text "File management functionality would go here." ]
            , ul []
              [ li
                [ HA.css
                    [ Css.display Css.flex_ ]
                ]
                [ span
                    [ HA.css
                      [ Css.flex3 Css.zero (Css.num 1) (Css.pct 100)
                      , Css.whiteSpace Css.nowrap
                      , Css.overflow Css.hidden
                      , Css.textOverflow Css.ellipsis
                      ]
                    ]
                    [ text "Unsaved graph" ]
                , span
                    [ HA.css
                        [ Css.flex2 Css.zero Css.zero
                        , Css.cursor Css.pointer
                        , Css.marginRight (Css.px 8)
                        ]
                    , HA.title "Edit graph name"
                    ]
                    [ text "🖋️" ]
                , span
                    [ HA.css
                        [ Css.flex2 Css.zero Css.zero
                        , Css.cursor Css.pointer
                        ]
                    , HA.title "Save graph"
                    ]
                    [ text "💾" ]
                ]
              -- , li [] [ text "📄 style.css" ]
              -- , li [] [ text "📁 src/" ]
              -- , li [] [ text "📁 tests/" ]
              ]
            ]
        
        Just SearchIcon ->
          div []
            [ h3 [] [ text "Search" ]
            , input 
              [ HA.type_ "text"
              , HA.placeholder "Search in files..."
              ] []
            , p [] [ text "Search results would appear here." ]
            ]
        
        Just GitIcon ->
          div []
            [ h3 [] [ text "Source Control" ]
            , p [] [ text "Git integration would go here." ]
            , div [ HA.class "panel-content" ]
              [ p [] [ text "• 3 changes" ]
              , p [] [ text "• 1 staged file" ]
              , p [] [ text "• main branch" ]
              ]
            ]
        
        Just TestsIcon ->
          div []
            [ h3 [] [ text "Debug Console" ]
            , p [] [ text "Debugging tools would go here." ]
            , div [ HA.class "panel-content panel-content--monospace" ]
              [ p [] [ text "> Ready to debug" ]
              , p [] [ text "> Breakpoints: 0" ]
              ]
            ]
        
        Just ExtensionsIcon ->
          div []
            [ h3 [] [ text "Extensions" ]
            , p [] [ text "Extension management would go here." ]
            , div [ HA.class "panel-content" ]
              [ p [] [ text "🔧 Elm Language Support" ]
              , p [] [ text "🎨 Dracula Theme" ]
              , p [] [ text "📝 Auto Format" ]
              ]
            ]
        
        Nothing ->
          text ""
    ]

viewHorizontalSplitter : Html Msg
viewHorizontalSplitter =
  div 
    [ HA.class "horizontal-splitter"
    , onMouseDown StartDraggingHorizontalSplitter
    ]
    []

viewRightSection : Model -> Html Msg
viewRightSection model =
  div 
    [ HA.class "right-section" ]
    [ viewRightTopPanel model
    , if model.rightBottomPanelOpen then
        div []
          [ viewVerticalSplitter
          , viewRightBottomPanel model
          ]
      else
        -- Show a minimal splitter when panel is closed
        viewCollapsedVerticalSplitter
    ]

viewRightTopPanel : Model -> Html Msg
viewRightTopPanel model =
  let
    (width, height) = model.rightTopPanelDimensions
  in
  div 
    [ HA.class "right-top-panel" ]
    [ -- For now, display dimensions as requested in comments
      div 
        [ HA.class "right-top-panel__content" ]
        [ Html.Styled.fromUnstyled
            ( FDG.view model.forceDirectedGraph
              |> Html.map ForceDirectedMsg
            )
        ]
    ]

viewVerticalSplitter : Html Msg
viewVerticalSplitter =
  div 
    [ HA.class "vertical-splitter"
    , onMouseDown StartDraggingVerticalSplitter
    ]
    [ div 
      [ HA.class "vertical-splitter__handle"
      , onClick ToggleBottomPanel
      ]
      []
    ]

viewCollapsedVerticalSplitter : Html Msg
viewCollapsedVerticalSplitter =
  div 
    [ HA.class "collapsed-vertical-splitter"
    , onClick ToggleBottomPanel
    ]
    [ div 
      [ HA.class "collapsed-vertical-splitter__handle" ]
      []
    ]

viewRightBottomPanel : Model -> Html Msg
viewRightBottomPanel model =
  div 
    [ HA.class "right-bottom-panel"
    , HA.style "height" (String.fromFloat model.rightBottomPanelHeight ++ "px")
    ]
    [ viewBottomPanelHeader model
    , viewBottomPanelContent model
    ]

viewTestPanelButtons : Model -> List (Html Msg)
viewTestPanelButtons model =
  [ button
    [ HA.class (getActionButtonClass model.executionState RunExecution)
    , onClick RunExecution
    , HA.disabled (model.executionState == ExecutionComplete || not (FDG.canExecute model.forceDirectedGraph))
    , HA.title "Run"
    ]
    [ text "▶️" ]
  , button
    [ HA.class (getActionButtonClass model.executionState ResetExecution)
    , onClick ResetExecution
    , HA.disabled (model.executionState == Ready || model.executionState == NotReady)
    , HA.title "Reset"
    ]
    [ text "⏹️" ]
  , button
    [ HA.class (getActionButtonClass model.executionState StepThroughExecution)
    , onClick StepThroughExecution
    , HA.disabled (model.executionState == ExecutionComplete || not (FDG.canExecute model.forceDirectedGraph))
    , HA.title "Step-through"
    ]
    [ text "⏭️" ]
  , button
    [ HA.class (getActionButtonClass model.executionState SaveTest)
    , onClick SaveTest
    , HA.disabled (String.isEmpty model.testPanelContent)
    , HA.title "Save test"
    ]
    [ text "💾" ]
  ]

viewDescriptionPanelButtons : Model -> List (Html Msg)
viewDescriptionPanelButtons model =
  []

viewBottomPanelHeader : Model -> Html Msg
viewBottomPanelHeader model =
  let
    buttons =
      ( case model.selectedBottomPanel of
          AddTestPanel ->
            viewTestPanelButtons model
          EditDescriptionPanel ->
            viewDescriptionPanelButtons model
      )
  in
  div 
    [ HA.classList
        [ ("bottom-panel__header", True)
        , ("open", not <| List.isEmpty buttons)
        ]
    ]
    [ div 
      [ HA.class "bottom-panel__actions" ]
      buttons
    ]

executionText : Model -> Html a
executionText { forceDirectedGraph, testPanelContent } =
  case forceDirectedGraph.execution of
    Nothing ->
      text "" -- eheh?! I should never be here!
    Just result ->
      case result of
        EndOfInput (Accepted _) ->
          p
            [ HA.class "output-line" ]
            [ text "✅ " 
            , strong [] [ text testPanelContent ]
            , text " accepted."
            ]
        EndOfInput (Rejected _) ->
          p
            [ HA.class "output-line" ]
            [ text "❌ " 
            , strong [] [ text testPanelContent ]
            , text " rejected."
            ]
        EndOfComputation (Accepted d) ->
          p
            [ HA.class "output-line" ]
            [ text "⛓️ Full input ("
            , strong [] [ text testPanelContent ]
            , text ") could not be handled. Computation terminated in an '✅ Accepting' state."
            ]
        EndOfComputation (Rejected d) ->
          p
            [ HA.class "output-line" ]
            [ text "⛓️ Full input ("
            , strong [] [ text testPanelContent ]
            , text ") could not be handled. Computation terminated in a '❌ Rejecting' state."
            ]
        CanContinue (Accepted { transitionsTaken, remainingData }) ->
          p
            [ HA.class "output-line" ]
            [ text <| "🟢 Continuing execution; " ++ String.fromInt (List.length transitionsTaken)
                ++ " paths taken, " ++ String.fromInt (List.length remainingData)
                ++ " characters remain to be matched.  Currently in an '✅ Accepting' state."
            ]
        CanContinue (Rejected { transitionsTaken, remainingData }) ->
          p
            [ HA.class "output-line" ]
            [ text <| "🟢 Continuing execution; " ++ String.fromInt (List.length transitionsTaken)
                ++ " paths taken, " ++ String.fromInt (List.length remainingData)
                ++ " characters remain to be matched.  Currently in a '❌ Rejecting' state."
            ]
        x ->
          p
            [ HA.class "output-line" ]
            [ text <| "🦗 Bug!  " ++ Debug.toString x ++ ".  You should never see this message.  I need to figure out what just happened here…"
            ]

viewAddTestPanelContent : Model -> Html Msg
viewAddTestPanelContent model =
  case model.executionState of
    Ready ->
      textarea 
        [ HA.class "right-bottom-panel__textarea"
        , HA.value model.testPanelContent
        , onInput UpdateTestPanelContent
        , HA.placeholder "Enter your test input here"
        , HA.disabled <| Maybe.Extra.isJust model.forceDirectedGraph.currentOperation
        ]
        []

    NotReady ->
      div
        []
        [ div 
            [ HA.class "notready-output" ]
            [ p [ HA.class "output-line" ] [ text "All changes must be committed or undone before you can execute." ] ]
        , textarea
            [ HA.class "right-bottom-panel__textarea"
            , HA.value model.testPanelContent
            , onInput UpdateTestPanelContent
            , HA.placeholder "Enter your test input here"
            , HA.disabled <| Maybe.Extra.isJust model.forceDirectedGraph.currentOperation
            ]
            []
        ]
    
    ExecutionComplete ->
      div 
        [ HA.class "execution-output" ]
        [ executionText model ]
    StepThrough ->
      div 
        [ HA.class "debug-output" ]
        [ executionText model ]

viewEditDescriptionPanelContent : Model -> Html Msg
viewEditDescriptionPanelContent model =
  textarea 
    [ HA.class "right-bottom-panel__textarea"
    , HA.value model.descriptionPanelContent
    , onInput UpdateDescriptionPanelContent
    , HA.placeholder "What does this computation do?"
    , HA.disabled <| Maybe.Extra.isJust model.forceDirectedGraph.currentOperation
    ]
    []

viewBottomPanelContent : Model -> Html Msg
viewBottomPanelContent model =
  div 
    [ HA.class "bottom-panel__content" ]
    [  div 
      [ HA.class "bottom-panel__titlebar" ]
      [ div
        [ HA.class "bottom-panel__title" ]
        [ text (getBottomPanelTitle model.selectedBottomPanel model.executionState) ]
      , div
        [ HA.class "bottom-panel__tab-buttons" ]
        [ button
          [ HA.classList
              [ ("tab-button", True)
              , ("tab-button--selected", model.selectedBottomPanel == AddTestPanel)
              ]
          , onClick <| SelectBottomPanel AddTestPanel
          , HA.title "Add test"
          ]
          [ text "🧪" ]
        , button
          [ HA.classList
              [ ("tab-button", True)
              , ("tab-button--selected", model.selectedBottomPanel == EditDescriptionPanel)
              ]
          , onClick <| SelectBottomPanel EditDescriptionPanel
          , HA.title "Describe computation"
          ]
          [ text "🗃️" ]
        ]
      ]
    , case model.selectedBottomPanel of
        AddTestPanel ->
          viewAddTestPanelContent model
        EditDescriptionPanel ->
          viewEditDescriptionPanelContent model
    ]

getBottomPanelTitle : BottomPanel -> ExecutionState -> String
getBottomPanelTitle panel state =
  case panel of
    AddTestPanel ->
      case state of
        Ready -> "Testing // Ready"
        ExecutionComplete -> "Testing // Execution complete" -- show execution output
        StepThrough -> "Testing // Step-through"
        NotReady -> "Testing // Not ready"
    EditDescriptionPanel ->
      "Describe computation"

getActionButtonClass : ExecutionState -> Msg -> String
getActionButtonClass currentState buttonAction =
  let
    baseClass = "action-button"
    activeClass = case (currentState, buttonAction) of
      (ExecutionComplete, RunExecution) -> " action-button--active"
      (StepThrough, StepThroughExecution) -> " action-button--active"
      _ -> ""
  in
  baseClass ++ activeClass

viewStatusBar : Model -> Html Msg
viewStatusBar model =
  div 
    [ HA.class "status-bar" ]
    [ span [] [ text (getStatusMessage model.executionState) ]
    , div 
      [ HA.class "status-bar__section" ]
      [ button
        [ HA.class (if model.rightBottomPanelOpen then "status-bar__button status-bar__button--active" else "status-bar__button")
        , onClick ToggleBottomPanel
        ]
        [ text "Terminal" ]
      ]
    , span 
      [ HA.class "status-bar__section--right" ]
      [ text ("Viewport: " ++ String.fromFloat (Tuple.first model.mainPanelDimensions) ++ " × " ++ String.fromFloat (Tuple.second model.mainPanelDimensions)) ]
    ]

getStatusMessage : ExecutionState -> String
getStatusMessage state =
  case state of
    Ready -> "Ready"
    ExecutionComplete -> "Running..."
    StepThrough -> "Debug Mode"
    NotReady -> "Uncommitted"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ FDG.subscriptions
        model.rightTopPanelDimensions
        model.forceDirectedGraph
      |> Sub.map ForceDirectedMsg
    , BE.onResize (\w h -> OnResize (toFloat w, toFloat h))
    , if model.isDraggingHorizontalSplitter || model.isDraggingVerticalSplitter then
        Sub.batch
          [ BE.onMouseMove (D.map2 MouseMove (D.field "clientX" D.float) (D.field "clientY" D.float))
          , BE.onMouseUp (D.succeed StopDragging)
          ]
      else
        Sub.none
    ]