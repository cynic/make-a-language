module Main exposing (..)

import Browser
import Browser.Events as BE
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph
import Automata.Data exposing (AutomatonGraph)
import Basics.Extra exposing (..)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Css exposing (..)
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

type alias Model =
  { forceDirectedGraph : ForceDirectedGraph.Model
  , mainPanelDimensions : ( Float, Float )
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
    model =
      D.decodeValue decodeDimensions flags
      |> Result.map
        (\dim ->
          { forceDirectedGraph = ForceDirectedGraph.receiveWords [] dim
          , mainPanelDimensions = dim
          }
        )
      |> Result.withDefault
        { forceDirectedGraph = ForceDirectedGraph.receiveWords [] ( 0, 0 )
        , mainPanelDimensions = (0, 0)
        }
  in
    ( model, Cmd.none )

-- UPDATE

type Msg
  = ForceDirectedMsg ForceDirectedGraph.Msg
  | ViewportResizeTrigger
  | OnResize (Float, Float)
  | SetMouseOver Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  -- stub
  ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  -- The interface is split into "left" and "right" parts, with a common status
  -- bar that runs underneath both.  The colors are inspired by the well-known
  -- "Dracula" theme, and the styling reflects a lighter aesthetic with rounded
  -- corners.  A thin bezel surrounds the entire interface.  The status bar is
  -- at least 1.8em in height.
  -- 
  -- On the left, there is a vertical (unremovable) "icon-bar" similar to the one
  -- in Visual Studio, currently filled with 5 random icons taken from HeroIcons.
  -- When the mouse moves over an icon, that icon is highlighted in some way and
  -- the cursor changes to a hand.  Clicking on icons will result in different
  -- Msg values being sent back to Elm.
  -- 
  -- When any icon is clicked, the left panel is considered to be "open" and it is
  -- displayed.  That icon remains highlighted.  The left panel has different
  -- (stub) HTML in it, depending on which icon was clicked.  When the highlighted
  -- icon is clicked again, the panel is closed.  If a different icon is clicked
  -- while the panel is open, then it becomes the highlighted icon and the (stub)
  -- HTML for that icon is displayed in the panel, which remains open. Whenever
  -- the left panel is "open", a movable splitter allows the user to change how
  -- much horizontal space is dedicated to left and right panels.  The minimum
  -- size of the open left panel is 100px, and the maximum size is 50% of the
  -- viewport width.
  --
  -- The right panel is split into "top" and "bottom" halves, which are separated
  -- by a movable splitter.  An interactable area on the splitter allows the bottom
  -- half to be completely hidden or "closed".  When the bottom half is open, its
  -- minimim height is 6em, and the maximum height is 50% of the viewport height.
  -- The bottom half is `contenteditable` and if the content is too large to fit,
  -- then vertical and/or horizontal scrollbars will appear.
  --
  -- The width and height of the top half is tracked at all times and changes
  -- to them result in `update` being called and the model's values being altered.
  -- The content for this panel comes from a `view` function that is defined
  -- elsewhere, and which is based on the `forceDirectedGraph` field of the Model.
  -- For now, the panel can display a light-grey / off-white background with HTML
  -- showing the width and height of the top half of the right panel.
  -- 
  div [] []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ ForceDirectedGraph.subscriptions
        model.mainPanelDimensions
        model.forceDirectedGraph
      |> Sub.map ForceDirectedMsg
    ]