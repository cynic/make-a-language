module Main exposing (main)

{- You know how the editor on the Elm website has two side-by-side panels that
can be resized? This is a rough implementation of that sort of thing.

APPROACH:
  1. Have a normal "mousedown" event on the drag zone.
  2. When a drag begins, listen for global onMouseMove and onMouseUp events.
  3. Check which buttons are down on mouse moves to detect a weird scenario.
-}


import Browser
import Browser.Events as BE
import Html.Styled exposing (..)
--import Html.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph
import DAWG
import Basics.Extra exposing (..)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Css exposing (..)
import Html.Styled.Attributes as HA exposing (css)

-- MAIN


main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Dimensions =
  { width : Float
  , height : Float
  }

type alias LayoutDimensions =
  { viewport : Dimensions
  , leftPanel : Dimensions
  , rightPanel : Dimensions
  }

type alias LayoutConfiguration =
  { leftRightSplitterWidth : Float
  , leftRightSplitPercentage : Float
  }

type alias Model =
  { dragState : DragState
  , text : String
  , forceDirectedGraph : ForceDirectedGraph.Model
  , dimensions : LayoutDimensions
  , layoutConfiguration : LayoutConfiguration
  }

type DragState
  = Static
  | Moving

decodeDimensions : D.Decoder Dimensions
decodeDimensions =
  D.map2
    (\w h -> { width = w, height = h })
    (D.field "width" D.float)
    (D.field "height" D.float)

viewportDimensionsToLayoutDimensions : Dimensions -> LayoutConfiguration -> LayoutDimensions
viewportDimensionsToLayoutDimensions viewport config =
  { viewport = viewport
  , leftPanel =
      Dimensions
        (viewport.width * config.leftRightSplitPercentage - config.leftRightSplitterWidth / 2.0)
        viewport.height
  , rightPanel =
      Dimensions
        (viewport.width * (1 - config.leftRightSplitPercentage) - config.leftRightSplitterWidth / 2.0)
        viewport.height
  }

initialLayoutConfig : LayoutConfiguration
initialLayoutConfig =
  { leftRightSplitterWidth = 10
  , leftRightSplitPercentage = 0.2
  }

updateLayout : Float -> Model -> Model
updateLayout fraction model =
  let
    config = model.layoutConfiguration
    newConfig = { config | leftRightSplitPercentage = fraction }
    newDimensions = viewportDimensionsToLayoutDimensions model.dimensions.viewport newConfig
    newForcesGraph =
      ForceDirectedGraph.update
        (ForceDirectedGraph.ViewportUpdated
          ( newDimensions.rightPanel.width - 10
          , newDimensions.rightPanel.height - 10
          )
        )
        model.forceDirectedGraph
  in
    { model
      | layoutConfiguration = newConfig
      , dimensions = newDimensions
      , forceDirectedGraph = newForcesGraph
    }

init : E.Value -> (Model, Cmd Msg)
init flags =
  D.decodeValue decodeDimensions flags
  |> Result.map
    (flip viewportDimensionsToLayoutDimensions initialLayoutConfig
    >>
    \layout ->
      ( { dragState = Static
        , text = ""
        , forceDirectedGraph =
            ForceDirectedGraph.init
              DAWG.empty
              ( layout.rightPanel.width - 10
              , layout.rightPanel.height - 10
              )
            |> Tuple.first
        , dimensions = layout
        , layoutConfiguration = initialLayoutConfig
        }
      , Cmd.none
      )
    )
  |> Result.withDefault
    ( { dragState = Static
      , text = ""
      , forceDirectedGraph =
          ForceDirectedGraph.init
            DAWG.empty
            (0, 0)
          |> Tuple.first
      , dimensions = viewportDimensionsToLayoutDimensions (Dimensions 0.0 0.0) initialLayoutConfig
      , layoutConfiguration = initialLayoutConfig
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
  | ViewportResizeTrigger
  | OnResize (Float, Float)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart ->
      ( { model | dragState = Moving }
      , Cmd.none
      )

    DragMove isDown fraction ->
      ( { model | dragState = if isDown then Moving else Static }
        |> updateLayout fraction
      , Cmd.none
      )

    DragStop fraction ->
      ( { model | dragState = Static }
        |> updateLayout fraction
      , Cmd.none
      )

    TextInput text ->
      ( { model
          | text = text
          , forceDirectedGraph =
              ForceDirectedGraph.update
                (ForceDirectedGraph.GraphUpdated <| DAWG.fromWords <| String.split "\n" text)
                model.forceDirectedGraph
        }
      , Cmd.none
      )

    ViewportResizeTrigger ->
      ( model
      , Browser.Dom.getViewport
        |> Task.perform (\x -> OnResize (x.viewport.width, x.viewport.height))
      )

    OnResize (width, height) ->
      let
        currentDimensions = model.dimensions
      in
        ( { model
            | dimensions = { currentDimensions | viewport = Dimensions width height }
          }
          |> updateLayout model.layoutConfiguration.leftRightSplitPercentage
        , Cmd.none
        ) 

    ForceDirectedMsg msg_ ->
      ( { model | forceDirectedGraph = ForceDirectedGraph.update msg_ model.forceDirectedGraph }
      , Cmd.none
      )

-- VIEW

view : Model -> Html Msg
view model =
  let
    {- The "user-select" and "pointer-event" properties are "none" when resizing,
    ensuring that text does not get highlighted as the mouse moves.
    -}
    pointerEvents_ =
      case model.dragState of
        Static -> auto
        Moving -> none
  in
    div -- outer div that wraps everything.
      [ css
          [ margin (px 0)
          , padding (px 0)
          , Css.width (vw 100)
          , Css.height (vh 100)
          ] 
      ]
      -- left panel
      [ div
          [ HA.id "left-panel"
          , css
              [ width (px <| model.dimensions.leftPanel.width - model.layoutConfiguration.leftRightSplitterWidth / 2.0)
              , height (px model.dimensions.leftPanel.height)
              , userSelect pointerEvents_
              , pointerEvents pointerEvents_
              , backgroundColor (rgb 255 25 255)
              , position absolute
              , top (px 0)
              , left (px 0)
              ]
          ]
          [ textarea
              [ css
                  [ width (px <| model.dimensions.leftPanel.width - 20)
                  , height (px <| model.dimensions.leftPanel.height - 20)
                  , resize none
                  , border (px 0)
                  , padding4 (px 5) (px 0) (px 5) (px 10)
                  , margin (px 5)
                  , borderRadius4 (px 8) (px 0) (px 0) (px 8)
                  , backgroundColor (rgb 90 200 120)
                  , fontSize (px 24)
                  , fontFamilyMany
                      ["Baskerville", "Libre Baskerville", "Consolas", "Cascadia Code", "Fira Code"]
                      serif
                  ]
              , onInput TextInput
              ]
              []
          ]
      -- splitter
      , div
          [ HA.id "left-right-splitter"
          , css
              [ position absolute
              , top (px 5)
              , left (px <| model.dimensions.leftPanel.width - model.layoutConfiguration.leftRightSplitterWidth / 2.0)
              , width (px model.layoutConfiguration.leftRightSplitterWidth)
              , height (px <| model.dimensions.viewport.height - 10)
              , backgroundColor (rgb 200 200 0)
              , cursor colResize
              , zIndex (int 10) -- needed???
              ]
          , on "mousedown" (D.succeed DragStart)
          ]
          []
      -- right panel
      , div
          [ HA.id "right-panel"
          , css
              [ width (px <| model.dimensions.rightPanel.width - 10)
              , height (px <| model.dimensions.rightPanel.height - 10)
              , userSelect pointerEvents_
              , pointerEvents pointerEvents_
              , backgroundColor (rgb 150 150 150)
              , position absolute
              , top (px 0)
              , left (px <| model.dimensions.leftPanel.width + model.layoutConfiguration.leftRightSplitterWidth / 2.0)
              , borderRadius4 (px 0) (px 8) (px 8) (px 0)
              , margin2 (px 5) (px 0)
              ]
          ]
          [ ForceDirectedGraph.view model.forceDirectedGraph
            |> fromUnstyled
            |> Html.Styled.map ForceDirectedMsg
          ]
      ]

-- VIEW DRAG ZONE


{- This does a few tricks to create an invisible drag zone:

  1. "z-index" is a high number so that this node is in front of both panels.
  2. "width" is 10px so there is something to grab onto.
  3. "position" is absolute so the "width" does not disrupt the panels.
  4. "margin-left" is -5px such that this node overhangs both panels.

You could avoid the 4th trick by setting "left" to "calc(50vw - 5px)" but I
do not know if there is a strong benefit to one approach or the other.
-}

-- SUBSCRIPTIONS


{- We listen for the "mousemove" and "mouseup" events for the whole window.
This way we catch all events, even if they are not on our drag zone.

Listening for mouse moves is costly though, so we only listen if there is an
ongoing drag.

Regarding resize, the Elm browser .onResize will give us the WINDOW's width
and height, which includes borders and scrollbars.  We actually want the
html's width and height, which excludes those things.  So we trigger a
resize which then makes a DOM call to ask for the correct information.
-}
mainSubscriptions : Model -> Sub Msg
mainSubscriptions model =
  case model.dragState of
    Static ->
      BE.onResize (\_ _ -> ViewportResizeTrigger)

    Moving ->
      Sub.batch
        [ BE.onMouseMove (D.map2 DragMove decodeButtons decodeFraction)
        , BE.onMouseUp (D.map DragStop decodeFraction)
        , BE.onResize (\_ _ -> ViewportResizeTrigger)
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

