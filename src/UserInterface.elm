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
          [ HA.class "navigation-bar"
          , HA.css
              [ Css.width <| Css.px <| model.uiConstants.navigationBarWidth ]
          ]
          [ debugWidth model.uiConstants.navigationBarWidth
          , button
              [ HA.classList
                  [ ("navigation-icon", True)
                  , ("active", model.uiState.selected.sideBar == ComputationsIcon)
                  ]
              , HA.title "Computations"
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
    , div
        [ HA.classList
            [ ("sidebar-separator", True)
            , ("dragging", model.currentOperation == Just (Dragging DragHorizontalSplitter))
            , ( "draggable"
              , ( model.currentOperation == Nothing ||
                  model.currentOperation == Just (Dragging DragHorizontalSplitter)
                )
                && model.uiState.open.sideBar
              )
            , ("sidebar-collapsed", not model.uiState.open.sideBar)
            ]
        , HA.css
            [ Css.width <| Css.px <| model.uiConstants.splitterWidth ]
        , if model.uiState.open.sideBar then
            HE.onMouseDown (UIMsg <| StartDragging DragHorizontalSplitter)
          else
            HE.on "dummy" (D.fail "dummy event")
        ]
        [ if not model.uiState.open.sideBar then
            div [] []
          else
            div
              [ HA.class "separator-handle" ]
              []
        , button
            [ HA.classList
                [ ("collapse-button", True)
                ]
            , HA.title "Toggle sidebar"
            , HE.onClick (UIMsg <| ToggleAreaVisibility NavigatorsArea)
            ]
            [ svg
                [ Svg.Styled.Attributes.class "collapse-icon"
                , Svg.Styled.Attributes.viewBox "4 4 10 8"
                ]
                [ Svg.Styled.path
                    [ Svg.Styled.Attributes.d "M10 12L6 8l4-4" ]
                    []
                ]
            ]
        ]
    ]
