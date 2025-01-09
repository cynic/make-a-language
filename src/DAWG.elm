module DAWG exposing (..)
import Graph exposing (Graph)
import Set exposing (Set)
import Dict exposing (Dict)

type alias UniqueIdentifier = Int

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = Set (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias DAWG =
  { graph : Graph UniqueIdentifier Transition
  , nextId : UniqueIdentifier
  , firstVertex : UniqueIdentifier
  , lastVertices : Dict Char (Set UniqueIdentifier)
  }

notFinalState : Int
notFinalState = 0
isFinalState : Int
isFinalState = 1

empty : DAWG
empty =
  { graph = Graph.addVertex 0 Graph.empty
  , nextId = 1
  , firstVertex = 0
  , lastVertices = Dict.empty
  }

defaultTransition : Transition
defaultTransition = Set.singleton ('❓', notFinalState)

{-| Merges suffixes. If possible, returns the new node to transition to, and the number of transitions to remove. -}
mergeSuffixes : Char -> List Char -> Graph UniqueIdentifier Transition -> Dict Char (Set UniqueIdentifier) -> Maybe (UniqueIdentifier, Int)
mergeSuffixes char pathHere graph lastVertices =
  {- |Here we want to start at the set of leaf-common-vertices for this char.
      Then move backwards.  At the very least, we should be able to merge the
      final letter, right?  When we run out of matching vertices, then we're
      done.

      I should probably take into account the "final" transitions.  I'll
      handle those when I get to it…
  -}
  let
    reversed = Graph.reverseEdges graph
    maxPath : List Char -> UniqueIdentifier -> Int -> List (UniqueIdentifier, Int)
    maxPath remaining vertex count =
      -- stop conditions:
      -- 1. I hit a confluence.
      -- 2. I hit a terminal transition.
      -- 3. I run out of transitions.
      case remaining of
        [] -> [(vertex, count)]
        ch::rest ->
          -- find the edge(s) that lead away from here.
          case Graph.outgoingEdgesWithData vertex reversed of
            [] -> [(vertex, count)]
            [(v, edgeSet)] -> -- there is only one `src` node
              if Set.size edgeSet == 1 then -- yay, only one path back
                if Set.member (ch, 0) edgeSet then
                  -- keep going.
                  maxPath rest v (1+count)
                else
                  -- no transitions from here; we're done.
                  [(vertex, count)]
              else -- more than one path back; i.e. confluence
                [(vertex, count)]
            pathSet ->
              -- more than one path back.  HOWEVER, if one of them has a single
              -- edge that leads away, AND it matches my character, then let's
              -- go for it.
              List.filter (\(_, edgeSet) -> Set.size edgeSet == 1 && Set.member (ch, 0) edgeSet) pathSet
              |> List.concatMap (\(v, _) -> maxPath rest v (1+count))

  in
    Dict.get char lastVertices
    |> Maybe.andThen
      (\vertexSet ->
          Set.map (\v -> maxPath (char::pathHere) 0 v) vertexSet
          |> Set.toList
          |> List.concatMap identity
          |> List.filter (\(_, n) -> n > 0)
          |> List.maximum
      )


{-| Add a new vertex to lastVertices set. Only does this if the vertex is a leaf.
-}
addVertexToLastVertices : Char -> UniqueIdentifier -> Dict Char (Set UniqueIdentifier) -> Graph UniqueIdentifier Transition -> Dict Char (Set UniqueIdentifier)
addVertexToLastVertices char endVertex lastVertices graph =
  if Graph.outgoingEdges endVertex graph == [] then
    Dict.update
      char
      (\v ->
          Just <| case v of
            Just existingSet -> Set.insert endVertex existingSet
            Nothing -> Set.singleton endVertex
      )
      lastVertices
  else
    lastVertices

{-| Given a starting vertex, add a new edge & vertex to the DAWG.  Returns the updated DAWG and the new vertex.

If the starting vertex is in the lastVertices set, then it is removed; it is no longer a leaf.  This does
not change its status as a final vertex.
-}
addNewEdge : Char -> Bool -> UniqueIdentifier -> DAWG -> (DAWG, UniqueIdentifier)
addNewEdge char isFinal vertex dawg =
  let
    graph =
      Graph.addEdge
        vertex
        dawg.nextId
        (Set.singleton (char, if isFinal then isFinalState else notFinalState))
        dawg.graph
    -- We are adding a new edge here.  So, if this node is considered to
    -- be a "last vertex" (more properly, a LEAF vertex), then we should
    -- actually remove it from the relevant set, lest it be considered a
    -- "real" suffix when it comes to suffix-merging.
    lastVertices =
      Dict.map (\_ a -> Set.remove dawg.nextId a) dawg.lastVertices
    nextId = dawg.nextId + 1
  in
    ( { dawg | graph = graph, nextId = nextId, lastVertices = lastVertices }
    , dawg.nextId
    )

type ConfluenceResult
  = NoConfluence -- only returned when we reach the start
  | FoundConfluence (UniqueIdentifier, List Char)
  | IncorrectPath

{-| Given a starting vertex, find the earliest confluence and the nodes to be added from it -}
findConfluence : UniqueIdentifier -> List Char -> DAWG -> Maybe (UniqueIdentifier, List Char)
findConfluence starting_vertex pathHere dawg =
  {- |There are a few cases to consider here as we trace back to the start,
      dst → src.  We are to find the EARLIEST confluence, so we need to
      trace all the way back to the start; the first vertex may present a
      confluence issue.
      
      (a) There is a completely straight line; there is no confluence.
          Every dst → src has one item only in the edgeSet.  This is the
          best (simplest) case and we return Nothing.

      (b) There is confluence dst → src where there is ONE edge and it
          has multiple items in the edgeSet.  Because there is only one
          edge, we select the `src` vertex and continue seeking along
          that path.

      (c) There is confluence dst → srcA, srcB, …, srcN where there is
          more than one edge.  Even worse, the dst → srcX edgeSets may
          each contain the appropriate character; consider nation-action-
          nativity and you will see this.  We have to follow all paths
          until we find the correct one.

      This sounds a bit complicated and expensive—especially (c)—but we
      typically use the discovery of confluence to figure out where to
      split a graph.  As a result, we end up doing less work later on,
      because we straighten the path at the earliest point.
  -}
  let
    reversed = Graph.reverseEdges dawg.graph
    follow : UniqueIdentifier -> List Char -> List Char -> ConfluenceResult
    follow vertex remaining seen =
      case (Graph.outgoingEdgesWithData vertex reversed, remaining) of
        ([], []) -> NoConfluence -- we must be at the 1st vertex now.
        ([(src, edgeSet)], ch::rest) ->
          -- there is only one node that we connect back to, but we may
          -- connect back along multiple edges.
          if Set.size edgeSet == 1 then
            if Set.member (ch, 0) edgeSet || Set.member (ch, 1) edgeSet then
              -- there is only one edge back, yay!  Follow it.
              follow src rest (ch::seen)
            else
              IncorrectPath -- we cannot match the forward path
          else
            case follow src rest (ch::seen) of
              NoConfluence -> FoundConfluence (src, seen) -- we are the earliest
              FoundConfluence v -> FoundConfluence v -- found an earlier one
              IncorrectPath -> IncorrectPath
        (paths, ch::rest) ->
          -- there is more than one node that we connect back to.
          -- In the ideal case, there is only one path that has our edge.
          case List.filter (\(_, edgeSet) -> Set.member (ch, 0) edgeSet || Set.member (ch, 1) edgeSet) paths of
            [(src, _)] ->
              -- yay, there is only one path that has our edge.
              -- Having said that, this is still a confluence.
              case follow src rest (ch::seen) of
                NoConfluence -> FoundConfluence (src, seen) -- we are the earliest
                FoundConfluence v -> FoundConfluence v -- found an earlier one
                IncorrectPath -> IncorrectPath
            [] ->
              -- no path has our edge.
              IncorrectPath
            possiblePaths ->
              -- more than one path has our edge.
              List.foldl
                (\(vtx, _) state ->
                  -- only one of these paths should return either NoConfluence
                  -- or FoundConfluence.  If we get IncorrectPath, do not
                  -- overwrite what the state is.
                  case follow vtx rest (ch::seen) of
                    NoConfluence -> NoConfluence
                    FoundConfluence v -> FoundConfluence v
                    IncorrectPath -> state
                )
                IncorrectPath
                possiblePaths
        (_, []) ->
          IncorrectPath -- longer than it can possibly be
  in
    case follow starting_vertex pathHere [] of
      NoConfluence -> Nothing
      FoundConfluence v -> Just v
      IncorrectPath -> Debug.log "ALGORITHM FAILURE—IncorrectPath should NEVER be returned!" Nothing

{-| Add a transition from a vertex node.

If the transition already exists to a NON-FINAL node, then do nothing.
However, if the transition exists to a FINAL node, then a new edge will
be added.  This is because a final node indicates a stopping-point, and
there is no way of knowing upfront that this is, in fact, going to be a
stopping-point.

Note that when we add a transition, we only ask about the vertex that
we are coming FROM; and we add a new vertex after that.  This ensures
that we cannot have cycles, because we cannot refer to previous vertices.
-}
addTransition : UniqueIdentifier -> Char -> Bool -> List Char -> DAWG -> (DAWG, UniqueIdentifier)
addTransition vertex char isFinal pathHere dawg =
  -- pathHere contains the path we took to get here, IN REVERSE ORDER.
  -- for example, it might contain ['r', 'e', 'd', 'e', 'r', 'f'] when `char` is 'i'.
  let
    existingEndVertex =
      Graph.outgoingEdgesWithData vertex dawg.graph
      |> List.filter (\(_, edgeData) -> Set.member (char, 0) edgeData || Set.member (char, 1) edgeData)
      |> List.head
  in
    -- There are several cases to consider.
    -- (1) If the graph is empty, just add it in.
    if Graph.size dawg.graph == 1 then
      addNewEdge char isFinal vertex dawg
    -- (2) If we already have this edge, then we're good.
    else
      case existingEndVertex of
        Just (endVertex, edgeSet) ->
          -- nothing to do; this is already here.
          -- BUT, if the edge is specified to be final, then we should…
          if isFinal then
            let
              -- update the graph to reflect that
              graph =
                Graph.updateEdge vertex endVertex (\_ -> Set.insert (char, 1) edgeSet) dawg.graph
              -- add this to the lastVertices, using the char as the key, IF
              -- there are no outgoing edges from here.  And there may be; consider
              -- freddy-fred-frederick, where we end on the first 'd' as well,
              -- but we extend past it to the two others.  In that case, the 'd'
              -- is a termination transition, but not a genuine leaf.
              lastVertices =
                addVertexToLastVertices char endVertex dawg.lastVertices dawg.graph
              -- if there are no outgoing edges from here, then we can also
              -- attempt a merge
              merged =
                if Graph.outgoingEdges endVertex dawg.graph == [] then
                  case mergeSuffixes char (char::pathHere) graph dawg.lastVertices of
                    Just (suffixVertex, n) ->
                      let
                        reversed = Graph.reverseEdges graph
                      in
                        
                else
                  graph
            in
              ( { dawg | graph = merged, lastVertices = lastVertices }
              , endVertex
              )
          else
            ( dawg, endVertex )
        Nothing ->
          {- |(3) the graph is not empty, and we don't have this transition
              yet.  Therefore, we must add it.  The only question is: where?
              Here we have two options:

                (a) if there are any outgoing edges already, …

                  (i) we can add it to an existing transition-set, thus causing
                      a "confluence" to form.  But we can only do that if we
                      know that the suffix is the same.  And we will only know
                      if the suffix is the same after we have reached the end
                      of the word.

                      So, at the end of the word, we must do a check. (see
                      `mergeSuffixes`)

                  (ii) we can split to a new edge.  This is the default.
                       behavior because of what is mentioned in (i).

                (b) if there are no outgoing edges, then what we do depends
                    on how we got here.  First, a definition from Watson
                    et al. (2000): a "confluence" is "any preexisting states
                    traversed by the common prefix that are already targets
                    of more than one in-transition".  In other words, a node
                    that has multiple outgoing edges, and lies upon the
                    path from firstVertex.

                  (i) if the path here was entirely straight—with no
                      "confluence" nodes—then we can just add it as a new
                      edge.  This is a straightforward extension, e.g.
                      fred-freddy or freddy-fred.  Why?  Because that's the
                      only way that we got here, so we know for a fact that
                      an extension of this is valid.

                  (ii) if the path here included any "confluence" nodes,
                       then we have to go backwards, to the first "confluence"
                       node, and do a split.  Think of frod-fred-freddy.
                       If we carried on with the 2nd "d" as a straightforward
                       extension, we'd end up accepting "froddy" as well,
                       incorrectly.  So we have to split along the prefix
                       that we entered upon, and clone transitions as we go.
                       Fortunately, because we store finality information
                       in TRANSITIONS rather than VERTICES, we avoid very
                       annoying problems of which vertices are final.

                       (a)(ii) stops new confluence nodes from forming.
                       However, a confluence node might already exist as an
                       existing vertex that we have traversed along the path,
                       and this is where (b)(ii) becomes relevant.              
          -}
          if Graph.outgoingEdges vertex dawg.graph == [] then
            -- we are in the situation of (b).  Check for confluence nodes.
            case findConfluence vertex pathHere dawg of
              Just (confluenceVertex, to_build) ->
                -- we are in the situation of (b)(ii).  We need to build a
                -- new path from here.
                List.foldl
                  (\ch (graph, pathVertex) -> addNewEdge ch False pathVertex graph)
                  (dawg, confluenceVertex)
                  to_build
              Nothing ->
                -- we are in the situation of (b)(i): there is no confluence.
                addNewEdge char isFinal vertex dawg
          else
            -- this is the situation for (a)(i) and (a)(ii)
            addNewEdge char isFinal vertex dawg

{-
{-| Change the transition-value between nodes.
-}
modifyTransition : UniqueIdentifier -> UniqueIdentifier -> Transition -> DAWG -> DAWG
modifyTransition a b value dawg =
  { dawg | graph = Graph.updateEdge a b (\_ -> value) dawg.graph }

toggleFinal : UniqueIdentifier -> DAWG -> DAWG
toggleFinal vertex dawg =
  {- this is actually more difficult than it might seem at first; we're not just toggling a bit.
     If the final-bit is set, then ALL of the chains leading to here must also accept this as a
     valid stopping-point.  If there is only one possible chain, then there is no issue, because
     we must want to toggle this one bit and that's fine.

     So, let us reverse the graph, and trace back to the start.  At every node that we land on,
     we can ask the original graph about the number of outgoing vertices.  If this is >1, …


  -}
  { dawg | graph = Graph.updateVertex vertex (\(id, isFinal) -> (id, not isFinal)) dawg.graph }
-}