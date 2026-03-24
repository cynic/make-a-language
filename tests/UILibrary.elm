module UILibrary exposing
  ( defaultModel
  , flags
  , flagsWithPackages
  , initWithPackages
  , withGraph
  , graphWithNodes
  , mainView
  , mainViewOrFail
  , mapMainView
  , step
  , step1
  , query
  , svgClass
  )
{-| Test harness for the UI-testing regimen described in `UI-Tests.md`.

    The app is a plain TEA loop, so UI tests drive it directly:

        Main.init flags  →  Main.update msg  →  Main.view model

    - `flags` builds deterministic JSON flags (fixed seed ⇒ deterministic
      UUIDs), so `defaultModel` is reproducible.
    - `withGraph` plants an `AutomatonGraph` into the main graph view using
      `Unsolved`, which bypasses the force-directed layout and therefore keeps
      node coordinates exactly as given.  This is essential for coordinate-
      sensitive tests.
    - `step` / `step1` feed `Msg` values through `Main.update`, discarding
      the (untestable) `Cmd`s.
    - `query` runs `Test.Html.Query` against the unstyled view.
    - `svgClass` is a class selector that works on SVG elements: SVG classes
      in this codebase are class *attributes*, and `Selector.class` matches
      only the HTML `className` property, so it would silently fail on SVG.
-}
import AutoDict
import Changes as C
import Data exposing (..)
import Graph exposing (NodeId)
import Html.Attributes
import Html.Styled
import Json.Encode as E
import Jsonify
import Main
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UILogic as UI
import Uuid
import Utility


{-| Deterministic flags for `Main.init`: an 800×600 viewport, a fixed seed,
    and no packages (so `init` creates one itself).
-}
flags : E.Value
flags =
  flagsWithPackages []


{-| Like `flags`, but with packages present.  Each package is encoded with
    `Jsonify.encodeGraphPackage`, which satisfies both decodes that
    `decodeFlags` performs on the `"packages"` field.
-}
flagsWithPackages : List GraphPackage -> E.Value
flagsWithPackages pkgs =
  E.object
    [ ( "width", E.float 800 )
    , ( "height", E.float 600 )
    , ( "initialSeed", E.int 42 )
    , ( "extendedSeeds", E.list E.int [ 1, 2, 3, 4, 5 ] )
    , ( "startTime", E.int 1600000000000 )
    , ( "packages", E.list Jsonify.encodeGraphPackage pkgs )
    ]


{-| A reproducible starting model (one graph view, one package, no
    interactions, idle capabilities).
-}
defaultModel : Model
defaultModel =
  Main.init flags
  |> Tuple.first


{-| Like `defaultModel`, but starting from the given packages.
-}
initWithPackages : List GraphPackage -> Model
initWithPackages pkgs =
  Main.init (flagsWithPackages pkgs)
  |> Tuple.first


{-| Replace the computation of the main graph view with the given graph.

    Uses `Unsolved` so that no force simulation runs and the node
    coordinates in `ag` survive untouched.  Capabilities are refreshed.
-}
withGraph : AutomatonGraph -> Model -> Model
withGraph ag model =
  let
    host =
      mainView model
      |> Maybe.map .host
      |> Maybe.withDefault (Dimension 452 392)
    gv =
      UI.makeGraphView model.mainGraphView Unsolved host False model.packages ag
  in
    C.upsertGraphView gv model
    |> UI.setCapabilities


{-| Feed a list of messages through `Main.update`, discarding Cmds.
-}
step : List Msg -> Model -> Model
step msgs model =
  List.foldl
    (\msg acc -> Tuple.first (Main.update msg acc))
    model
    msgs


{-| `step` with a single message.
-}
step1 : Msg -> Model -> Model
step1 msg model =
  step [ msg ] model


{-| The main graph view, if present.
-}
mainView : Model -> Maybe GraphView
mainView model =
  AutoDict.get model.mainGraphView model.graph_views


{-| The application view, ready for `Test.Html.Query` assertions.
-}
query : Model -> Query.Single Msg
query model =
  Html.Styled.toUnstyled (Main.view model)
  |> Query.fromHtml


{-| A class selector that works on SVG elements (exact match on the joined
    class attribute; TypedSvg joins classes in code order, so this is
    deterministic).
-}
svgClass : List String -> Selector.Selector
svgClass names =
  Selector.attribute
    (Html.Attributes.attribute "class" (String.join " " names))


{-| An `AutomatonGraph` with the given nodes (at the given coordinates) and
    no edges.  The first node is the root.
-}
graphWithNodes : List ( NodeId, Float, Float ) -> AutomatonGraph
graphWithNodes nodes =
  { graph =
      Graph.fromNodesAndEdges
        (List.map
          (\( id, x, y ) ->
            { id = id
            , label = (Data.entity id NoEffect) |> (\e -> { e | x = x, y = y })
            }
          )
          nodes
        )
        []
  , description = Nothing
  , root = nodes |> List.head |> Maybe.map (\( id, _, _ ) -> id) |> Maybe.withDefault 0
  }


{-| The main graph view, falling back to a placeholder view (built from an
    empty graph) if it is somehow absent.  Models built via this harness
    always have a main graph view.
-}
mainViewOrFail : Model -> GraphView
mainViewOrFail model =
  mainView model
  |> Maybe.withDefault
      (UI.makeGraphView (Utility.uuid_from_int 0) Unsolved (Dimension 452 392) False (AutoDict.empty Uuid.toString) Data.empty)


{-| Modify the main graph view in place (keeping its uuid).
-}
mapMainView : (GraphView -> GraphView) -> Model -> Model
mapMainView f model =
  C.updateGraphView model.mainGraphView f model
