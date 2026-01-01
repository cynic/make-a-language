module Changes exposing
  ( deletePackageFromModel
  , linkGraphViewToPackage
  , mapComputation
  , mapDrawingData
  , mapGraph
  , popInteraction
  , popMostRecentInteraction
  , pushInteractionForStack
  , removeViews
  , replaceInteraction
  , selectPackage
  , setMainView
  , convertTentativeLinkToPermanent
  , updateDrawingData
  , updateGraphView
  , upsertGraphView
  , updatePackageFromView
  )
import AutoDict
import Automata.Data exposing (..)
import Graph exposing (Graph(..))
import List.Extra as List
import Queries as Q
import Uuid exposing (Uuid)
import Automata.DFA as DFA

upsertGraphView : GraphView -> Model -> Model
upsertGraphView graph_view model =
  { model
    | graph_views =
        AutoDict.insert graph_view.id graph_view model.graph_views
  }

updateGraphView : Uuid -> (GraphView -> GraphView) -> Model -> Model
updateGraphView uuid f model =
  AutoDict.get uuid model.graph_views
  |> Maybe.map (\gv ->
    { model | graph_views = AutoDict.insert uuid (f gv) model.graph_views }
  )
  |> Maybe.withDefault model

updateDrawingData : Uuid -> (DrawingData -> DrawingData) -> Model -> Model
updateDrawingData view_uuid f model =
  { model
    | graph_views =
        AutoDict.update view_uuid
          (Maybe.map (\graph_view ->
            { graph_view
              | drawingData = f graph_view.drawingData
            }
          ))
          model.graph_views             
  }

{-| Called to turn phantoms (nodes + edges) into solid things
-}
convertTentativeLinkToPermanent : Uuid -> Model -> Model
convertTentativeLinkToPermanent view_uuid model =
{-
  To solidify all phantom nodes:
  - there must be no `.phantom_node`
  
-}
  updateDrawingData view_uuid
    (\drawingData ->
      { drawingData
        -- | selected_nodes = Set.empty
        | tentative_link = Nothing
        -- , highlighted_links =
        --     Set.remove (src, phantom_id) drawingData.highlighted_links
      }
    )
    model

setMainView : Uuid -> Model -> Model
setMainView uuid model =
  { model | mainGraphView = uuid }

selectPackage : Uuid -> Model -> Model
selectPackage uuid model =
  { model | selectedPackage = uuid }

{-| Push an interaction onto the an interaction-stack.  If no such interaction-stack
    exists, create one before pushing.
-}
pushInteractionForStack : Maybe Uuid -> InteractionState -> Model -> Model
pushInteractionForStack uuid interaction model =
  let
    new_recent_number =
      AutoDict.values model.interactionsDict
      |> List.maximumBy Tuple.first
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
  in
  { model
    | interactionsDict =
        AutoDict.update uuid
          ( Maybe.withDefault (0, [])
          >> (\(_, stack) -> Just (new_recent_number, interaction :: stack))
          )
          model.interactionsDict
  }

{-| Pop an interaction from a particular stack.  If there are no interactions left on the
    stack, then the stack disappears.
-}
popInteraction : Maybe Uuid -> Model -> Maybe (InteractionState, Model)
popInteraction uuid model =
  -- annoyingly, I can't use .update for this because I want to _also_ return the head…
  -- so, in most cases, there are going to be two key lookups.
  -- Oh well!

  -- get the stack
  case AutoDict.get uuid model.interactionsDict of
    Just (_, [h]) ->
      -- println "One interaction on this stack; removing the stack itself."
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.remove uuid model.interactionsDict }
        )
    Just (r, h :: t) ->
      -- if it has something, pop, and also return an updated model
      -- println "More than one interaction on this stack; removing the topmost interaction."
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.insert uuid (r - 1, t) model.interactionsDict }
        )
    _ ->
      -- println "Was called to pop, but there's nothing to pop!"
      -- if there's nothing, or there was no such dict, there's nothing
      -- to do
      Nothing

popMostRecentInteraction : Model -> Maybe (Maybe Uuid, InteractionState, Model)
popMostRecentInteraction model =
  Q.mostRecentInteraction model
  |> Maybe.andThen (\(uuid, _) ->
    popInteraction uuid model
    |> Maybe.map (\(interaction, model_) -> (uuid, interaction, model_))
  )

replaceInteraction : Maybe Uuid -> InteractionState -> Model -> Model
replaceInteraction uuid interaction model =
  popInteraction uuid model
  |> Maybe.map (\(_, model_) -> pushInteractionForStack uuid interaction model_)
  |> Maybe.withDefault model

removeViews : List Uuid -> Model -> Model
removeViews uuids model =
  { model
    | graph_views =
        List.foldl AutoDict.remove model.graph_views uuids
  }

mapDrawingData : (DrawingData -> DrawingData) -> GraphView -> GraphView
mapDrawingData f gv =
  { gv | drawingData = f gv.drawingData }

mapGraph : (Graph Entity Connection -> Graph Entity Connection) -> GraphView -> GraphView
mapGraph f gv =
  { gv
    | computation =
        let ag = gv.computation in
        { ag | graph = f ag.graph }
  }

mapComputation : (AutomatonGraph -> AutomatonGraph) -> GraphView -> GraphView
mapComputation f gv =
  { gv | computation = f gv.computation }

updatePackageFromView : Uuid -> Model -> Model
updatePackageFromView gv_uuid model =
  AutoDict.get gv_uuid model.graph_views
  |> Maybe.andThen (\graph_view ->
    graph_view.graphPackage
    |> Maybe.map (\pkg_uuid ->
      { model
        | packages =
            AutoDict.update pkg_uuid
              (Maybe.map (\pkg ->
                { pkg
                  | computation = graph_view.computation
                  -- run the tests
                  , tests =
                      AutoDict.map
                        (\_ entry ->
                          { entry
                            | result =
                                DFA.load entry.input (Q.resolutionDict model.packages) graph_view.computation
                                |> DFA.run
                                |> List.head
                                |> Maybe.andThen .finalResult
                                |> Maybe.withDefault (InternalError "no result received from test execution")
                          }
                        )
                        pkg.tests
                }
              ))
              model.packages
      }
    )
  )
  |> Maybe.withDefault model -- do nothing; there is no GV, or no link, or no package.

deletePackageFromModel : Uuid -> Model -> Model
deletePackageFromModel uuid model =
  { model | packages = AutoDict.remove uuid model.packages }

linkGraphViewToPackage : PackageDict -> Uuid -> GraphView -> GraphView
linkGraphViewToPackage packages package_uuid graph_view =
  if AutoDict.member package_uuid packages then
    { graph_view | graphPackage = Just package_uuid }
  else
    graph_view
