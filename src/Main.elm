module Main exposing (..)

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
import Html.Styled.Events exposing (..)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph
import Automata.Data exposing (AutomatonGraph)
import Automata.Verification
import Basics.Extra exposing (..)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Css exposing (..)
import Html.Styled.Attributes as HA exposing (css)
import List.Extra
import Automata.DFA

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

type alias Dimensions =
  { width : Float
  , height : Float
  }

type alias LayoutDimensions =
  { viewport : Dimensions
  , leftPanel : Dimensions
  , rightPanel : Dimensions
  , recognizedWords : Dimensions
  }

type alias LayoutConfiguration =
  { leftRightSplitterWidth : Float
  , leftRightSplitPercentage : Float
  }

type alias DAWGMetrics =
  { recognized : List String
  , numNodes : Int
  , numEdges : Int
  , combinable : List (List Int)
  }

type alias Model =
  { dragState : DragState
  , text : String
  , forceDirectedGraph : ForceDirectedGraph.Model
  , dimensions : LayoutDimensions
  , layoutConfiguration : LayoutConfiguration
  , metrics : DAWGMetrics
  , mouseIsOver : Bool
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
  let
    recognizedWordsHeight = 38
    rightPanelHeight = viewport.height - recognizedWordsHeight
  in
  { viewport = viewport
  , leftPanel =
      Dimensions
        (viewport.width * config.leftRightSplitPercentage - config.leftRightSplitterWidth / 2.0)
        viewport.height
  , rightPanel =
      Dimensions
        (viewport.width * (1 - config.leftRightSplitPercentage) - config.leftRightSplitterWidth / 2.0)
        rightPanelHeight
  , recognizedWords =
      Dimensions
        (viewport.width * (1 - config.leftRightSplitPercentage) - config.leftRightSplitterWidth / 2.0)
        recognizedWordsHeight
  }

initialLayoutConfig : LayoutConfiguration
initialLayoutConfig =
  { leftRightSplitterWidth = 5
  , leftRightSplitPercentage = 0.1
  }

updateLayout : Float -> Model -> Model
updateLayout fraction model =
  let
    config = model.layoutConfiguration
    newConfig = { config | leftRightSplitPercentage = fraction }
    newDimensions = viewportDimensionsToLayoutDimensions model.dimensions.viewport newConfig
    newForcesGraph =
      (ForceDirectedGraph.update
        (newDimensions.leftPanel.width + 10, 10)
        (ForceDirectedGraph.ViewportUpdated
          ( newDimensions.rightPanel.width - 10
          , newDimensions.rightPanel.height - 10
          )
        )
      )
      model.forceDirectedGraph
  in
    { model
      | layoutConfiguration = newConfig
      , dimensions = newDimensions
      , forceDirectedGraph = newForcesGraph
    }

defaultMetrics : { recognized : List a, numNodes : number, numEdges : number, combinable : List b }
defaultMetrics =
  { recognized = []
  , numNodes = 0
  , numEdges = 0
  , combinable = []
  }

defaultModel : LayoutDimensions -> Model
defaultModel dimensions =
  { dragState = Static
  , text = ""
  , forceDirectedGraph =
      ForceDirectedGraph.receiveWords [] ( dimensions.viewport.width, dimensions.viewport.height )
  , dimensions = dimensions
  , layoutConfiguration = initialLayoutConfig
  , metrics = defaultMetrics
  , mouseIsOver = False
  }

insaneLayoutDimensions : LayoutDimensions
insaneLayoutDimensions = -- rubbish, nonsensical values.
  { viewport = { width = 0, height = 0 }
  , leftPanel = { width = 0, height = 0 }
  , rightPanel = { width = 0, height = 0 }
  , recognizedWords = { width = 0, height = 0 }
  }

init : E.Value -> (Model, Cmd Msg)
init flags =
  D.decodeValue decodeDimensions flags
  |> Result.map
    (flip viewportDimensionsToLayoutDimensions initialLayoutConfig
    >>
    \layout ->
      ( defaultModel layout
      , Cmd.none
      )
    )
  |> Result.withDefault ( defaultModel insaneLayoutDimensions , Cmd.none )


-- UPDATE

calcMetrics : AutomatonGraph a -> DAWGMetrics
calcMetrics g =
  let
    words = Automata.Verification.verifiedRecognizedWords g
  in
    { recognized = words
    , numNodes = Automata.Verification.numNodes g
    , numEdges = Automata.Verification.numEdges g
    , combinable = Automata.Verification.minimality g
    }


type Msg
  = DragStart
  | DragMove Bool Float
  | DragStop Float
  | TextInput String
  | ForceDirectedMsg ForceDirectedGraph.Msg
  | ViewportResizeTrigger
  | OnResize (Float, Float)
  | SetMouseOver Bool

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
      let
        words = String.split "\n" text |> List.filter ((/=) "")
        dfa = Automata.DFA.fromWords words
        dfa_graph = Automata.DFA.toAutomatonGraph dfa
      in
      ( { model
          | text = text
          , forceDirectedGraph =
              ForceDirectedGraph.receiveWords
                words
                ( model.dimensions.rightPanel.width
                , model.dimensions.rightPanel.height - 10
                )
          , metrics =
              case words of
                [] -> defaultMetrics
                _ -> calcMetrics dfa_graph
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
      ( { model
          | forceDirectedGraph =
              ForceDirectedGraph.update
                (model.dimensions.leftPanel.width + 10, 10)
                msg_
                model.forceDirectedGraph
        }
      , Cmd.none
      )

    SetMouseOver isOver ->
      ( { model | mouseIsOver = isOver }
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
          , backgroundColor (hsl 1 0 0.85) -- off-white / light gray
          , Css.width (vw 100)
          , Css.height (vh 100)
          , gridTemplateColumns <| trackList (px model.dimensions.leftPanel.width) [px model.layoutConfiguration.leftRightSplitterWidth, fr 1]
          , display grid_
          ] 
      ]
      -- left panel
      [ div
          [ HA.id "left-panel"
          , css
              [ userSelect pointerEvents_
              , pointerEvents pointerEvents_
              -- , backgroundColor (rgb 255 25 255)
              , margin4 (px 0) (px 0) (px 0) (px 5)
              , display flex_
              , flexDirection column
              ]
          ]
          [ textarea
              [ css
                  [ flexGrow (num 2)
                  , resize none
                  , border (px 0)
                  , margin2 (px 5) (px 0)
                  , padding4 (px 4) (px 0) (px 4) (px 10)
                  , borderRadius4 (px 8) (px 0) (px 0) (px 0)
                  -- , backgroundColor (rgb 90 200 120)
                  , border3 (px 1) solid (rgb 128 128 128)
                  , fontSize (px 18)
                  , fontFamilyMany
                      ["Baskerville", "Libre Baskerville", "Consolas", "Cascadia Code", "Fira Code"]
                      serif
                  ]
              , HA.placeholder "Enter words here, one per line"
              , onInput TextInput
              , onMouseOver (SetMouseOver True)
              , onMouseOut (SetMouseOver False)
              , HA.disabled <| Basics.not model.mouseIsOver
              ]
              [ Html.Styled.text model.text ]
          -- , textarea
          --     [ css
          --         [ flexGrow (num 1)
          --         , resize none
          --         , border (px 0)
          --         , padding4 (px 4) (px 0) (px 4) (px 10)
          --         , borderRadius4 (px 0) (px 0) (px 0) (px 8)
          --         , marginBottom (px 5)
          --         -- , backgroundColor (rgb 90 200 120)
          --         , border3 (px 1) solid (rgb 128 128 128)
          --         , fontSize (px 18)
          --         , fontFamilyMany
          --             ["Baskerville", "Libre Baskerville", "Consolas", "Cascadia Code", "Fira Code"]
          --             serif
          --         ]
          --     , HA.placeholder "Algebraic input here"
          --     , onInput AlgebraicTextInput
          --     ]
          --     [ Html.Styled.text model.algebraic ]
          -- , div
          --     [ css
          --         [ height (Css.em 2.2)
          --         , flexGrow (num 0)
          --         ]
          --     ]
          --     [ input
          --         [ HA.type_ "checkbox"
          --         , HA.id "useAlgebraic"
          --         , onCheck SetAlgebraicUse
          --         ]
          --         []
          --     , label
          --         [ for "checkbox" ]
          --         [ Html.Styled.text "Use algebraic?" ]

          --     ]
          ]
      -- splitter
      , div
          [ HA.id "left-right-splitter"
          , css
              [ width (px <| model.layoutConfiguration.leftRightSplitterWidth)
              , backgroundColor (rgb 160 160 160)
              , cursor colResize
              , margin2 (px 5) (px 0)
              --, zIndex (int 10) -- needed???
              ]
          , on "mousedown" (D.succeed DragStart)
          ]
          []
      -- right panel
      , div
          [ HA.id "right-panel"
          , css
              -- + 1px for border on either side = 12
              [ userSelect pointerEvents_
              , pointerEvents pointerEvents_
              -- , backgroundColor (rgb 150 150 150)
              , display flex_
              , flexDirection column
              , marginRight (px 5)
              , overflowX auto
              , overflowY hidden
              ]
          ]
          [ div
              [ css
                  [ flexBasis (px model.dimensions.rightPanel.height)
                  , flexGrow (num 1)
                  , border3 (px 1) solid (rgb 128 128 128)
                  , borderRadius4 (px 0) (px 8) (px 0) (px 0)
                  , margin2 (px 5) (px 0)
                  ]
              ]
              [ ForceDirectedGraph.view model.forceDirectedGraph
                |> fromUnstyled
                |> Html.Styled.map ForceDirectedMsg
              ]
        -- recognized words zone
        , let
            isSame = (String.split "\n" model.text |> List.Extra.unique |> List.filter ((/=) "") |> List.sort) == model.metrics.recognized
            (fgcolor, bgcolor) = if isSame then (rgb 0 0 0, rgb 208 240 192) else (rgb 255 255 255, rgb 236 88 0)
          in
          div
            [ HA.id "recognized-words-zone"
            , css
                [ userSelect pointerEvents_
                , pointerEvents pointerEvents_
                , border3 (px 1) solid (rgb 128 128 128)
                , borderRadius4 (px 0) (px 0) (px 8) (px 0)
                , padding2 (px 0) (px 4)
                , display flex_
                , alignItems center
                , overflowX auto
                , marginBottom (px 5)
                , whiteSpace nowrap
                , backgroundColor bgcolor
                , color fgcolor
                ]
            ]
            [ let
                wordsRecognized = String.join " ◉ " <| model.metrics.recognized
                minimality =
                  case model.metrics.combinable of
                    [] -> "🟢"
                    xs -> "🟠 (can combine: " ++ (List.map (\combinable -> "[" ++ (List.map String.fromInt combinable |> String.join ", ") ++ "]") >> String.join "; ") xs ++ ") "
                metrics =
                  String.fromInt model.metrics.numNodes ++ " nodes, " ++ String.fromInt model.metrics.numEdges ++ " edges, " ++ String.fromInt (List.length model.metrics.recognized) ++ " words."
              in
              Html.Styled.text <| wordsRecognized ++ "      🛈: " ++ minimality ++ " " ++ metrics ]
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
    , ForceDirectedGraph.subscriptions
        (model.dimensions.leftPanel.width + 10, 10)
        model.forceDirectedGraph
      |> Sub.map ForceDirectedMsg
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

