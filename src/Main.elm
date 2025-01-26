-- create default skeleton
module Main exposing (main)


{- You know how the editor on the Elm website has two side-by-side panels that
can be resized? This is a rough implementation of that sort of thing.

APPROACH:
  1. Have a normal "mousedown" event on the drag zone.
  2. When a drag begins, listen for global onMouseMove and onMouseUp events.
  3. Check which buttons are down on mouse moves to detect a weird scenario.
-}


import Browser
import Browser.Events as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Svg exposing (svg)
import Svg.Attributes as SA exposing (width, height, viewBox)
import ForceDirectedGraph


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dragState : DragState
  , text : String
  , forceDirectedGraph : ForceDirectedGraph.Model
  }


type DragState
  = Static Float
  | Moving Float


init : () -> (Model, Cmd Msg)
init _ =
  ( { dragState = Static 0.5
    , text = ""
    , forceDirectedGraph = ForceDirectedGraph.init () |> Tuple.first
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DragStart
  | DragMove Bool Float
  | DragStop Float
  | TextInput String
  | ForceDirectedMsg ForceDirectedGraph.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart ->
      ( { model | dragState = Moving (toFraction model.dragState) }
      , Cmd.none
      )

    DragMove isDown fraction ->
      ( { model | dragState = if isDown then Moving fraction else Static (toFraction model.dragState) }
      , Cmd.none
      )

    DragStop fraction ->
      ( { model | dragState = Static fraction }
      , Cmd.none
      )

    TextInput text ->
      ( { model | text = text }
      , Cmd.none
      )

    ForceDirectedMsg msg_ ->
      ( { model | forceDirectedGraph = ForceDirectedGraph.update msg_ model.forceDirectedGraph }
      , Cmd.none
      )


toFraction : DragState -> Float
toFraction dragState =
  case dragState of
    Static fraction -> fraction
    Moving fraction -> fraction



-- VIEW


view : Model -> Html Msg
view model =
  let
    fraction = Basics.max (0.2) <| Basics.min 0.5 (toFraction model.dragState)
    pointerEvents = toPointerEvents model.dragState
  in
  div
    [ style "margin" "0"
    , style "padding" "0"
    , style "width" "100vw"
    , style "height" "100vh"
    , style "font-family" "monospace"
    , style "display" "flex"
    , style "flex-direction" "row"
    ]
    [ viewPanel "#222" "#ddd" fraction pointerEvents
        [ textarea
            [ style "width" (String.fromFloat (100 * fraction) ++ "vw")
            , style "height" "100%"
            , style "resize" "none"
            , style "border" "none"
            , style "padding" "10px"
            , style "background-color" "#f4f4f4"
            , onInput TextInput
            ]
            []
        ]
    , viewDragZone fraction
    , viewPanel "#ddd" "#222" (1 - fraction) pointerEvents
        [ ForceDirectedGraph.view model.forceDirectedGraph
          |> Html.map ForceDirectedMsg
        ]
        -- [ svg
        --     [ style "flex-grow" "1"
        --     , style "background-color" "white"
        --     -- , style "border-left" "2px solid #ccc"
        --     , SA.width "100%"
        --     , SA.height "100%"
        --     , viewBox "0 0 100 100"
        --     ]
        --     []
        -- ]
    ]


toPointerEvents : DragState -> String
toPointerEvents dragState =
  case dragState of
    Static _ -> "auto"
    Moving _ -> "none"



{- The "user-select" and "pointer-event" properties are "none" when resizing,
ensuring that text does not get highlighted as the mouse moves.
-}
viewPanel : String -> String -> Float -> String -> List (Html msg) -> Html msg
viewPanel background foreground fraction pointerEvents content =
  div
    [ style "width" (String.fromFloat (100 * fraction) ++ "vw")
    , style "height" "100vh"
    , style "user-select" pointerEvents
    , style "pointer-events" pointerEvents
    --
    , style "background-color" background
    , style "color" foreground
    -- , style "font-size" "3em"
    --
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]
    (content)



-- VIEW DRAG ZONE


{- This does a few tricks to create an invisible drag zone:

  1. "z-index" is a high number so that this node is in front of both panels.
  2. "width" is 10px so there is something to grab onto.
  3. "position" is absolute so the "width" does not disrupt the panels.
  4. "margin-left" is -5px such that this node overhangs both panels.

You could avoid the 4th trick by setting "left" to "calc(50vw - 5px)" but I
do not know if there is a strong benefit to one approach or the other.
-}
viewDragZone : Float -> Html Msg
viewDragZone fraction =
        -- , div
        --     [ style "width" "10px"
        --     , style "background-color" "#ccc"
        --     , style "cursor" "ew-resize"
        --     , style "height" "100%"
        --     , onMouseDown StartResizing
        --     ]
        --     []
  div
    [ style "position" "absolute"
    , style "top" "0"
    , style "left" (String.fromFloat (100 * fraction) ++ "vw")
    , style "width" "10px"
    , style "height" "100vh"
    , style "margin-left" "-5px"
    , style "cursor" "col-resize"
    , style "z-index" "10"
    , style "background-color" "#ccc"
    , on "mousedown" (D.succeed DragStart)
    ]
    []



-- SUBSCRIPTIONS


{- We listen for the "mousemove" and "mouseup" events for the whole window.
This way we catch all events, even if they are not on our drag zone.

Listening for mouse moves is costly though, so we only listen if there is an
ongoing drag.
-}
mainSubscriptions : Model -> Sub Msg
mainSubscriptions model =
  case model.dragState of
    Static _ ->
      Sub.none

    Moving _ ->
      Sub.batch
        [ E.onMouseMove (D.map2 DragMove decodeButtons decodeFraction)
        , E.onMouseUp (D.map DragStop decodeFraction)
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ mainSubscriptions model
    , ForceDirectedGraph.subscriptions model.forceDirectedGraph |> Sub.map ForceDirectedMsg
    ]

{- The goal here is to get (mouse x / window width) on each mouse event. So if
the mouse is at 500px and the screen is 1000px wide, we should get 0.5 from this.

Getting the mouse x is not too hard, but getting window width is a bit tricky.
We want the window.innerWidth value, which happens to be available at:

    event.currentTarget.defaultView.innerWidth

The value at event.currentTarget is the document in these cases, but this will
not work if you have a <section> or a <div> with a normal elm/html event handler.
So if currentTarget is NOT the document, you should instead get the value at:

    event.currentTarget.ownerDocument.defaultView.innerWidth
                        ^^^^^^^^^^^^^
-}
decodeFraction : D.Decoder Float
decodeFraction =
  D.map2 (/)
    (D.field "pageX" D.float)
    (D.at ["currentTarget","defaultView","innerWidth"] D.float)


{- What happens when the user is dragging, but the "mouse up" occurs outside
the browser window? We need to stop listening for mouse movement and end the
drag. We use MouseEvent.buttons to detect this:

    https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons

The "buttons" value is 1 when "left-click" is pressed, so we use that to
detect zombie drags.
-}
decodeButtons : D.Decoder Bool
decodeButtons =
  D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)

