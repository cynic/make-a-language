module Changes exposing
  ( solidifyPhantoms
  )
import Automata.Data exposing (..)
import Uuid exposing (Uuid)
import Graph exposing (NodeId)
import Set
import AutoDict

upsertGraphView : Uuid -> GraphView -> Model -> Model
upsertGraphView uuid graph_view model =
  { model
    | graph_views =
        AutoDict.insert uuid { graph_view | id = uuid } model.graph_views
  }

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
