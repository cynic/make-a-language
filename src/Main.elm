module Main exposing (..)

import Browser
import Browser.Events as BE
import Html.Styled exposing (Html, div, h3, p, ul, li, input, textarea, span, toUnstyled, text, button)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph
import Automata.Data exposing (AutomatonGraph)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Html.Styled.Attributes as HA
import List.Extra
import Automata.DFA

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
  | DebugIcon
  | ExtensionsIcon

type alias Model =
  { forceDirectedGraph : ForceDirectedGraph.Model
  , mainPanelDimensions : ( Float, Float )
  , leftPanelOpen : Bool
  , selectedIcon : Maybe LeftPanelIcon
  , leftPanelWidth : Float
  , rightBottomPanelOpen : Bool
  , rightBottomPanelHeight : Float
  , rightTopPanelDimensions : ( Float, Float )
  , bottomPanelContent : String
  , isDraggingHorizontalSplitter : Bool
  , isDraggingVerticalSplitter : Bool
  , mousePosition : ( Float, Float )
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
    initialRightTopHeight = height - 30 - 100  -- 30px status bar, 100px bottom panel
  in
    ( { forceDirectedGraph = ForceDirectedGraph.receiveWords [] ( initialRightTopWidth, initialRightTopHeight )
      , mainPanelDimensions = ( width, height )
      , leftPanelOpen = False
      , selectedIcon = Nothing
      , leftPanelWidth = 250
      , rightBottomPanelOpen = True
      , rightBottomPanelHeight = 100
      , rightTopPanelDimensions = ( initialRightTopWidth, initialRightTopHeight )
      , bottomPanelContent = "// Welcome to the automaton editor\n// Type your code here..."
      , isDraggingHorizontalSplitter = False
      , isDraggingVerticalSplitter = False
      , mousePosition = ( 0, 0 )
      }
    , Task.perform (\_ -> NoOp) (Task.succeed ())
    )

-- UPDATE

type Msg
  = ForceDirectedMsg ForceDirectedGraph.Msg
  | ViewportResizeTrigger
  | OnResize (Float, Float)
  | SetMouseOver Bool
  | ClickIcon LeftPanelIcon
  | StartDraggingHorizontalSplitter
  | StartDraggingVerticalSplitter
  | StopDragging
  | MouseMove Float Float
  | ToggleBottomPanel
  | UpdateBottomPanelContent String
  | UpdateRightTopDimensions Float Float
  | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ForceDirectedMsg fdMsg ->
      let
        newFdModel = ForceDirectedGraph.update (0, 0) fdMsg model.forceDirectedGraph
      in
      ( { model | forceDirectedGraph = newFdModel }
      , Cmd.none
      )

    ViewportResizeTrigger ->
      ( model
      , Task.perform (\viewport -> OnResize (viewport.viewport.width, viewport.viewport.height)) Browser.Dom.getViewport
      )

    OnResize (width, height) ->
      let
        newModel = { model | mainPanelDimensions = (width, height) }
        (newRightTopWidth, newRightTopHeight) = calculateRightTopDimensions newModel
      in
      ( { newModel | rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight) }
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
        (newRightTopWidth, newRightTopHeight) = calculateRightTopDimensions { model | leftPanelOpen = newLeftPanelOpen }
      in
      ( { model 
        | leftPanelOpen = newLeftPanelOpen
        , selectedIcon = newSelectedIcon
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
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
          (newRightTopWidth, newRightTopHeight) = calculateRightTopDimensions { newModel | leftPanelWidth = newLeftPanelWidth }
        in
        ( { newModel 
          | leftPanelWidth = newLeftPanelWidth
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          }
        , Cmd.none
        )
      else if model.isDraggingVerticalSplitter then
        let
          (_, viewportHeight) = model.mainPanelDimensions
          minHeight = 96  -- 6em ≈ 96px
          maxHeight = viewportHeight / 2
          statusBarHeight = 30
          newBottomHeight = clamp minHeight maxHeight (viewportHeight - y - statusBarHeight)
          (newRightTopWidth, newRightTopHeight) = calculateRightTopDimensions { newModel | rightBottomPanelHeight = newBottomHeight }
        in
        ( { newModel 
          | rightBottomPanelHeight = newBottomHeight
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          }
        , Cmd.none
        )
      else
        ( newModel, Cmd.none )

    ToggleBottomPanel ->
      let
        newBottomPanelOpen = not model.rightBottomPanelOpen
        (newRightTopWidth, newRightTopHeight) = calculateRightTopDimensions { model | rightBottomPanelOpen = newBottomPanelOpen }
      in
      ( { model 
        | rightBottomPanelOpen = newBottomPanelOpen
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        }
      , Cmd.none
      )

    UpdateBottomPanelContent content ->
      ( { model | bottomPanelContent = content }, Cmd.none )

    UpdateRightTopDimensions width height ->
      ( { model | rightTopPanelDimensions = (width, height) }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )

calculateRightTopDimensions : Model -> ( Float, Float )
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
  in
  ( rightTopWidth, rightTopHeight )

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
    , viewIcon SearchIcon "🔍" model
    , viewIcon GitIcon "🌿" model
    , viewIcon DebugIcon "🐛" model
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
            [ h3 [] [ text "File Explorer" ]
            , p [] [ text "File management functionality would go here." ]
            , ul []
              [ li [] [ text "📄 main.elm" ]
              , li [] [ text "📄 style.css" ]
              , li [] [ text "📁 src/" ]
              , li [] [ text "📁 tests/" ]
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
        
        Just DebugIcon ->
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
        [ h3 [] [ text "Force Directed Graph View" ]
        , p [] [ text ("Width: " ++ String.fromFloat width ++ "px") ]
        , p [] [ text ("Height: " ++ String.fromFloat height ++ "px") ]
        , p [ HA.class "text--muted" ] 
          [ text "The ForceDirectedGraph.view would be rendered here." ]
        ]
      -- TODO: Integrate ForceDirectedGraph.view here
      -- Html.Styled.fromUnstyled (ForceDirectedGraph.view model.forceDirectedGraph |> Html.map ForceDirectedMsg)
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
    [ textarea 
      [ HA.class "right-bottom-panel__textarea"
      , HA.value model.bottomPanelContent
      , onInput UpdateBottomPanelContent
      ]
      []
    ]

viewStatusBar : Model -> Html Msg
viewStatusBar model =
  div 
    [ HA.class "status-bar" ]
    [ span [] [ text "Ready" ]
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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ ForceDirectedGraph.subscriptions
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