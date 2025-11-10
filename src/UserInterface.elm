module UserInterface exposing (..)
import Html.Styled exposing
  (Html, div, text, button)
import Html.Styled.Events as HE
import Json.Decode as D
import Automata.Data exposing (..)
import Html.Styled.Attributes as HA
import Css
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Automata.Data exposing (..)

type alias Msg = Main_Msg
type alias Model = Main_Model

debugViewDimensions : Bool
debugViewDimensions = True

debugElement : String -> String -> Html a
debugElement otherClass s =
  if debugViewDimensions then
    div
      [ HA.class "debug-parent" ]
      [ div
        [ HA.class <| "debug-dimensions " ++ otherClass ]
        [ text s ]
      ]
  else
    div
      []
      []

debugDimensions : Dimensions -> Html a
debugDimensions (w, h) =
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
    [ if not model.uiState.open.sideBar then
        div [] []
      else
        div
          [ HA.class "navigation-bar" ]
          [ button
              [ HA.classList
                  [ ("navigation-icon", True)
                  , ("active", model.uiState.selected.sideBar == ComputationsIcon)
                  ]
              , HA.title "Computations"
              -- , if model.uiState.selected.sideBar == ComputationsIcon then
              --     HE.onClick (UIMsg <| ToggleAreaVisibility NavigatorsArea)
              --   else
              --     HE.on "dummy" (D.fail "dummy event")
              ]
              [ text "📁"]
          , button
              [ HA.classList
                  [ ("navigation-icon", True)
                  , ("active", model.uiState.selected.sideBar == TestsIcon)
                  ]
              , HA.title "Tests"
              ]
              [ text "🧪" ]
          ]
    , if not model.uiState.open.sideBar then
        div [] []
      else
        div
          [ HA.class "sidebar"
          , HA.css
              [ Css.width <| Css.px <|
                  if model.uiState.open.sideBar then
                    Automata.Data.width model.uiState.dimensions.sideBar
                  else
                    0
              ]
          ]
          [ debugDimensions model.uiState.dimensions.sideBar
          , div
              [ HA.class "sidebar-content" ]
              [ text "Hello world" ]
          ]
    ]

viewSplitter : Int -> SplitterMovement -> Maybe InteractionState -> Bool -> Html Main_Msg
viewSplitter zIdx movement interactionStack areaOpen =
  let
    (movementClass, targetArea) =
      case movement of
        LeftRight ->
          ( "leftright", NavigatorsArea )
        UpDown ->
          ( "updown", ToolsArea )
    isDragging = interactionStack == Just (DraggingSplitter movement)
    draggable = (interactionStack == Nothing || isDragging) && areaOpen
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
    if areaOpen then
      div
        [ HA.classList
            [ ("splitter-separator " ++ movementClass, True)
            , ("dragging", isDragging)
            , ( "draggable", draggable)
            ]
        , HA.css
            [ Css.zIndex (Css.int zIdx)
            ]
        , HE.onMouseDown (UIMsg <| StartDraggingSplitter movement)
        ]
        [ div
            [ HA.class <| "separator-handle " ++ movementClass ]
            [ button
                [ HA.class <| "collapse-button " ++ movementClass
                , HA.title "Collapse"
                , HE.onClick (UIMsg <| ToggleAreaVisibility targetArea)
                ]
                [ collapseIcon ]
            ]
        ]
    else
      button
        [ HA.class <| "collapse-button " ++ movementClass ++ " collapsed"
        , HA.title "Expand"
        , HE.onClick (UIMsg <| ToggleAreaVisibility targetArea)
        , HA.css
            [ Css.zIndex (Css.int <| zIdx * 10)
            ]
        ]
        [ collapseIcon ]


viewToolsArea : Model -> Html Msg
viewToolsArea model =
  div
    [ HA.class "tools-container" ]
    [ if not model.uiState.open.bottomPanel then
        div [] []
      else
        div
          [ HA.class "tools-bar" ]
          [ button
              [ HA.classList
                  [ ("tool-icon", True)
                  , ("active", model.uiState.selected.bottomPanel == TestingToolIcon)
                  ]
              , HA.title "Testing"
              ]
              [ text "🔬"]
          , button
              [ HA.classList
                  [ ("tool-icon", True)
                  , ("active", model.uiState.selected.bottomPanel == MetadataToolIcon)
                  ]
              , HA.title "Tests"
              ]
              [ text "📝" ]
          ]
    , if not model.uiState.open.bottomPanel then
        div [] []
      else
        div
          [ HA.class "tools-area"
          , HA.css
              [ Css.height <| Css.px <|
                  if model.uiState.open.bottomPanel then
                    Automata.Data.height model.uiState.dimensions.bottomPanel
                  else
                    0
              -- , Css.width <| Css.px <|
              --     if model.uiState.open.bottomPanel then
              --       Automata.Data.width model.uiState.dimensions.bottomPanel - 36
              --     else
              --       0
              ]
          ]
          [ debugDimensions model.uiState.dimensions.bottomPanel
          , div
              [ HA.class "tool-content" ]
              [ text "Hello world" ]
          ]
    ]
