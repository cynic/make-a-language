module Changes exposing
  ( selectPackage
  , setMainView
  , solidifyPhantoms
  , updateGraphView
  , upsertGraphView
  )
import Automata.Data exposing (..)
import Uuid exposing (Uuid)
import Graph exposing (NodeId)
import Set
import AutoDict

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
solidifyPhantoms : Uuid -> NodeId -> NodeId -> Model -> Model
solidifyPhantoms view_uuid src phantom_id model =
{-
  To solidify all phantom nodes:
  - there must be no `.phantom_node`
  
-}
  updateDrawingData view_uuid
    (\drawingData ->
      { drawingData
        -- | selected_nodes = Set.empty
        | phantom_node = Nothing
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