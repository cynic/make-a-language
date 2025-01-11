module DAWG exposing (..)
import Graph exposing (Graph)
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra as List exposing (Step(..))
import Html exposing (br)

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

-- a list of chars, and the destination node for each one.
-- This is stored in reverse order, e.g. a sequence of characters
-- 'f', 'r', 'e', 'd' added sequentially might end up as
-- [('d', 9), ('e', 8), ('r', 7), ('f', 6)].  There is no need
-- to ever store the starting vertex—we only need to store the
-- destination that each goes to—because we know the starting
-- vertex already.
type alias Breadcrumbs = List Breadcrumb
type alias Breadcrumb = (Char, UniqueIdentifier)

{-| Given breadcrumbs, return the edges from those breadcrumbs.

The furthest edges are returned first.

    breadcrumbsToForwardEdges [('d', 9), ('e', 8), ('r', 7), ('f', 6)]
    --> [(8,9),(7,8),(6,7),(0,6)]
-}
breadcrumbsToForwardEdges : Breadcrumbs -> List (UniqueIdentifier, UniqueIdentifier)
breadcrumbsToForwardEdges breadcrumbs =
  List.reverse breadcrumbs
  |> List.map (\(_, v) -> v)
  |> List.foldl (\crumb (seen, last) -> ((last, crumb)::seen, crumb)) ([], 0)
  |> Tuple.first
  |> List.reverse -- not strictly necessary, but it will return them in a nice order

{-| Given breadcrumbs, return the characters for each one.

Just as with breadcrumbs, they are returned in order of furthest edge first.

    charsOfBreadcrumbs [('d', 9), ('e', 8), ('r', 7), ('f', 6)]
    --> ['d', 'e', 'r', 'f']

-}
charsOfBreadcrumbs : Breadcrumbs -> List Char
charsOfBreadcrumbs breadcrumbs =
  List.map Tuple.first breadcrumbs

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

-- defaultTransition : Transition
-- defaultTransition = Set.singleton ('❓', notFinalState)

{-| Merges suffixes of the graph.

`lastVertices` may or may not contain the breadcrumbs.
-}
mergeSuffixes : Breadcrumbs -> Graph UniqueIdentifier Transition -> Dict Char (Set UniqueIdentifier) -> Graph UniqueIdentifier Transition
mergeSuffixes pathHere graph lastVertices =
  {- |Here we want to start at the set of leaf-common-vertices for this char.
      Then move backwards.  At the very least, we should be able to merge the
      final letter, right?  When we run out of matching vertices, then we're
      done.

      I should probably take into account the "final" transitions.  I'll
      handle those when I get to it…
  -}
  let
    reversed = Graph.reverseEdges graph
    characters = charsOfBreadcrumbs pathHere
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
    longestPath : Maybe (UniqueIdentifier, Int)
    longestPath =
      List.head characters
      |> Maybe.andThen (\char -> Dict.get char lastVertices)
      |> Maybe.andThen
        (\vertexSet ->
            Set.map (\v -> maxPath characters 0 v) vertexSet
            |> Set.toList
            |> List.concatMap identity
            -- if the path-length is zero, no merging is possible.
            -- if the path-length is the same as pathHere, then we're looking
            -- at the exact word; we can't merge into ourselves, so ignore it.
            |> List.filter (\(_, n) -> n > 0 && n < List.length pathHere)
            |> List.maximum
        )
    -- contains (those to remove, those remaining)
    edgesToRemove =
      Maybe.map
        (\(_, n) -> List.take n (breadcrumbsToForwardEdges pathHere))
        longestPath
    joiningCrumb : Maybe (Char, UniqueIdentifier)
    joiningCrumb =
      Maybe.andThen
        (\(_, n) -> List.drop n pathHere |> List.head)
        longestPath
    sansEdges : Maybe (Graph UniqueIdentifier Transition)
    sansEdges =
      Maybe.map
        (List.foldl (\(a, b) g -> Graph.removeEdge a b g) graph)
        edgesToRemove
    redirectedGraph : Maybe (Graph UniqueIdentifier Transition)
    redirectedGraph =
      Maybe.map3
        (\(ch, src) (dst, _) g ->
          Graph.addEdge src dst (Set.singleton (ch, 0)) g
        )
        joiningCrumb longestPath sansEdges
  in
    redirectedGraph |> Maybe.withDefault graph

{-| Add a new vertex to lastVertices set. Only does this if the vertex is a leaf.
-}
addVertexToLastVertices : Char -> UniqueIdentifier -> Graph UniqueIdentifier Transition -> Dict Char (Set UniqueIdentifier) -> Dict Char (Set UniqueIdentifier)
addVertexToLastVertices char endVertex graph lastVertices =
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
      Dict.map (\_ a -> Set.remove vertex a) dawg.lastVertices
      |> addVertexToLastVertices char dawg.nextId graph
    nextId = dawg.nextId + 1
  in
    ( { dawg | graph = graph, nextId = nextId, lastVertices = lastVertices }
    , dawg.nextId
    )

{-| Given a starting vertex, find the earliest confluence and the nodes to be added from it -}
findConfluence : Breadcrumbs -> DAWG -> Maybe (UniqueIdentifier, List Char)
findConfluence pathHere dawg =
  let
    edges = breadcrumbsToForwardEdges pathHere -- get the edges, from furthest-to-closest to start
    reversed = Graph.reverseEdges dawg.graph
  in
    List.reverse edges -- organize from closest-to-furthest from start
    |> List.stoppableFoldl
        (\(src,dst) (v, count) ->
          case Graph.getEdge src dst dawg.graph of
            Nothing ->
              Debug.log ("I tried to get an edge from " ++ String.fromInt src ++ " to " ++ String.fromInt dst ++ ", but it didn't exist.  THIS SHOULD *NOT* HAPPEN!")
                (Continue (Nothing, 0))
            Just edgeSet ->
              if Set.size edgeSet == 1 then
                if List.length (Graph.outgoingEdges dst reversed) == 1 then 
                  -- this is not a confluence node; move on.
                  Continue (Nothing, count+1)
                else
                  -- this is a confluence node.  Consider: action-nation-national
                  Stop (Just src, count)
              else
                -- this is the earliest confluence node.
                Stop (Just src, count)
        )
        (Nothing, 0)
    |>  (\(src, count) ->
          Maybe.andThen (\found -> Just (found, List.drop count <| charsOfBreadcrumbs pathHere)) src
        )

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
addTransition : UniqueIdentifier -> Char -> Bool -> Breadcrumbs -> DAWG -> (DAWG, UniqueIdentifier)
addTransition vertex char isFinal pathHere dawg =
  -- pathHere contains the path we took to get here, IN REVERSE ORDER, and the
  -- vertex identifiers that are the "forward" edges.  Since the very first
  -- vertex is the start vertex, we can translate this into a list of edges if
  -- we want to do so.
  -- For example, on the character level only, `pathHere` might contain
  -- ['r', 'e', 'd', 'e', 'r', 'f'] when `char` is 'i'.
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
                addVertexToLastVertices char endVertex dawg.graph dawg.lastVertices
              -- if there are no outgoing edges from here, then we can also
              -- attempt a merge
              merged =
                if Graph.outgoingEdges endVertex dawg.graph == [] then
                  mergeSuffixes ((char, endVertex)::pathHere) graph lastVertices
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
            case findConfluence pathHere dawg of
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

type IsThisFinal a
  = IsFinal a
  | NotFinal a

{-| Convenience method to add entire strings to the DAWG.
-}
addString : String -> DAWG -> DAWG
addString str dawg =
  case String.toList str of
    [] -> dawg
    characters ->
      let
-- addTransition : UniqueIdentifier -> Char -> Bool -> Breadcrumbs -> DAWG -> (DAWG, UniqueIdentifier)
        proceed crumbs node g remaining =
          case remaining of
            [] -> dawg
            [ch] ->
              addTransition node ch True crumbs g
              |> Tuple.first
            (ch::rest) ->
              let
                (dawg_, node_) = addTransition node ch False crumbs dawg
              in
                proceed ((ch, node_)::crumbs) node_ dawg_ rest
      in
        proceed [] dawg.firstVertex dawg characters

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  Debug.log txt
    { graph = Graph.verticesAndEdges dawg.graph
    , firstVertex = dawg.firstVertex
    , lastVertices = dawg.lastVertices
    , nextId = dawg.nextId
    }
  |> \_ -> dawg

type TransitionType
  = SameStream (Char, UniqueIdentifier)
  | AdditionalStart (Char, UniqueIdentifier)

type Following
  = SinglePath TransitionType
  | MultiplePaths (List TransitionType)
  | End

recognizedWords : DAWG -> List String
recognizedWords dawg =
  let
    reversed = Graph.reverseEdges dawg.graph
    analyzeTransition : UniqueIdentifier -> (Char, Int) -> TransitionType
    analyzeTransition src transition =
      case transition of
        (ch, 1) -> AdditionalStart (ch, src) 
        (ch, _) -> SameStream (ch, src)
    analyzeVertex : UniqueIdentifier -> Following
    analyzeVertex vertex =
      case Graph.outgoingEdgesWithData vertex reversed of
        [] -> End
        [(src, edgeSet)] ->
          case Set.toList edgeSet of
            [] -> End |> Debug.log "but this should never happen"
            [x] -> SinglePath (analyzeTransition src x)
            xs -> 
              MultiplePaths (List.map (analyzeTransition src) xs)
        manyPaths ->
          MultiplePaths <|
            List.concatMap
              (\(src, edgeSet) -> List.map (analyzeTransition src) (Set.toList edgeSet))
              manyPaths
    processTransition : List Char -> UniqueIdentifier -> TransitionType -> List String
    processTransition seen dst transition =
      case transition of
        SameStream (ch, nextVertex) ->
          fromRightToLeft (ch::seen) nextVertex
        AdditionalStart (ch, nextVertex) ->
          -- check: is this a leaf?  If not, then kick off a new one.
          case Graph.outgoingEdges dst dawg.graph of
            [] -> fromRightToLeft (ch::seen) nextVertex -- we are a leaf.
            _ -> fromRightToLeft (ch::seen) nextVertex ++ fromRightToLeft [ch] nextVertex
    fromRightToLeft : List Char -> UniqueIdentifier -> List String
    fromRightToLeft seen vertex =
      case analyzeVertex vertex of
        End -> [String.fromList seen]
        SinglePath v ->
          processTransition seen vertex v
        MultiplePaths transitions ->
          List.concatMap (processTransition seen vertex) transitions

  in
    dawg.lastVertices
    |> Dict.values
    |> List.concatMap
        (\v ->
          List.concatMap (fromRightToLeft []) (Set.toList v)
        )
    |> List.sort

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