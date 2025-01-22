module DAWG exposing (..)
import Graph exposing (Graph)
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Html exposing (br)
import Dict exposing (empty)

type alias UniqueIdentifier = Int

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias NodeConnections = Set (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias DAWG =
  { graph : Graph UniqueIdentifier NodeConnections
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


-- -- 'dst' is ALWAYS further from the start.
-- -- 'src' is ALWAYS closer to the start.
-- -- Of course, the two indicate direction.
-- type alias SingleTransition =
--   { src : UniqueIdentifier
--   , dst : UniqueIdentifier
--   , char : Char
--   , isFinalTransition : Bool
--   , distance_from_root : Int -- first transition is 0
--   }

-- type SuffixMergeAction
--   = RemoveCompletely SingleTransition -- remove edge AND vertex
--   | JoinVertices SingleTransition -- definitely create
--   | UpdateConnection SingleTransition -- definitely update
--   | UpdateOrCreate SingleTransition
--   -- check prefixes from both src values.  If match, execute.
--   | ConnectIfPrefixMatches (SingleTransition -> (UniqueIdentifier, UniqueIdentifier) -> SuffixMergeAction)
--   -- Go back one level, and connect.
--   | ConnectOneBack (SingleTransition -> (UniqueIdentifier, UniqueIdentifier) -> SuffixMergeAction)

-- -- type NodeAnalysis
-- --   = SplitForward -- case (1) or (2)
-- --   | SplitBackward
-- --   | TerminalNode
-- --   | StartNode -- case (3)

-- type MatchingBehaviour
--   = KeepMatchingAnd SuffixMergeAction
--   | StopMatchingAnd SuffixMergeAction
--   | KeepMatching
--   | StopMatching

-- mergeSuffixes5 : Set UniqueIdentifier -> Breadcrumbs -> DAWG -> DAWG
-- mergeSuffixes5 sameCharLeafVertices pathHere dawg =
--   let
--     characters = charsOfBreadcrumbs pathHere
--     reversed = Graph.reverseEdges dawg.graph
--     commonPathToStart : List Char -> List UniqueIdentifier -> UniqueIdentifier -> List UniqueIdentifier
--     commonPathToStart charList accumulator vertex =
--       case charList of
--         [] ->
--           if vertex == dawg.firstVertex then
--             List.reverse (dawg.firstVertex :: accumulator)
--           else
--             [] -- nope.  Did not lead to the start.
--         ch::rest ->
--           case Graph.outgoingEdgesWithData vertex reversed of
--             [] ->
--               -- we still have characters to match, but this is the end;
--               -- so, this is not a path to the start.
--               []
--             [(v, edgeSet)] ->
--               if Set.member (ch, 0) edgeSet || Set.member (ch, 1) edgeSet then
--                 commonPathToStart rest (v::accumulator) v
--               else
--                 []
--             paths ->
--               List.findMap
--                 (\(v_, _) ->
--                   case commonPathToStart rest accumulator v_ of
--                     [] -> Nothing
--                     foundPath -> Just foundPath
--                 )
--                 paths
--               |> Maybe.withDefault []
--     isSplitForward : UniqueIdentifier -> Bool
--     isSplitForward vertex =
--       List.length (Graph.outgoingEdges vertex dawg.graph) > 1
--     isRoot : UniqueIdentifier -> Bool
--     isRoot vertex =
--       vertex == dawg.firstVertex
--     is_first_edge : SingleTransition -> Bool
--     is_first_edge to_match =
--       to_match.distance_from_root == 0
--     edges_forward_from : UniqueIdentifier -> List (UniqueIdentifier, NodeConnections)
--     edges_forward_from vertex =
--       Graph.outgoingEdgesWithData vertex dawg.graph
--     edge_forward_containing : Char -> UniqueIdentifier -> Maybe (UniqueIdentifier, NodeConnections)
--     edge_forward_containing char source =
--       edges_forward_from source
--     -- you know what?  I don't think I'm following back nodes. 🤯.  I'm
--     -- following back TRANSITIONS and nodes.  A little subgraph!
--     -- And I should be checking: dst; src; edge-set;
--     -- to figure out what to do here.
--     -- Actually—I'm matching pairs, aren't I?  word-edges and graph-edges?
--     match : SingleTransition -> (UniqueIdentifier, UniqueIdentifier) -> MatchingBehaviour
--     match edge_to_match (dst, src) =
--       {-
--         1. If `src` is a split FORWARD.  I cannot attach there.
--            because the word WILL then become part of the split.  I can only
--            attach to a later node, so I will attach to `dst`.

--            EXCEPTION: when `src` is the start-node for the DAWG *AND* when
--            edge_to_match is 0-distance from the root.  In that case, the prefix
--            is empty.  We can form an edge, IF one does not already exist,
--            from root→dst using char.
--       -}
--         if isSplitForward src then
--           if isRoot src && edge_to_match |> is_first_edge then
--             StopMatchingAnd <| UpdateOrCreateConnection
--               { src = dawg.firstVertex
--               , dst = dst
--               , char = edge_to_match.char
--               , isFinalTransition = edge_to_match.isFinalTransition
--               , distance_from_root = 0
--               }
--           else
--             StopMatchingAnd <| UpdateOrCreateConnection
--               { src = edge_to_match.src
--               , dst = dst
--               , char = edge_to_match.char
--               , isFinalTransition = edge_to_match.isFinalTransition
--               , distance_from_root = 0
--               }
--       {-
--         5. I arrive at a node that is a confluence. I am now able to go ONE
--            level back, **IF** there is a common path back to the start.  This
--            is because adding another edge to that edgeSet does NOT result in
--            the acceptance of any other words.  However, I MUST then stop; I
--            cannot go more than ONE level back, otherwise I will accept words
--            that I should not accept.
--       -}
--       {-
--         2. If `dst` is a terminal node: similar to a split FORWARD node, if I
--            attach to a terminal node or anywhere before it, I will accept extra
--            words.  I must therefore attach after it.

--            (think about in future: I actually think this might be too strong.
--            I store termination on transitions, so I can surely go back—by ONE—
--            if I need to, and add to the edgeSet??)

--            (meh, the heck with it—I'm gonna try it out. I think it's viable!)

--            So:
--              - if the word does terminate here, AND if the terminating
--                transition isn't the same as my character; then I cannot merge
--                these two UNLESS the prefixes are the same.  So: if there is a
--                prefix path to the start, then we can merge.
--                  - When `src` is the start-node for the DAWG *AND* when
--                    edge_to_match is 0-distance from the root, then this is
--                    trivially true because there IS no prefix.  Otherwise, I can
--                    follow it back to the start and see; there's a function for
--                    that
--                If I can merge, then a merge entails this: connect word `src` to
--                graph `dst`, using the word's char and setting the termination on
--                that transition.
--              - if the word does terminate here, AND if the terminating
--                transition is the same as my character, then this is exactly
--                what I want and I can remove my word's src→dst transition
--                completely.
--              - if the word doesn't terminate here; AND if the terminating
--                transition isn't the same as my character; then I can go back
--                one level in my word, and connect its `dst` to the `src`, using
--                its char.

--                 For this, do I need the graph-`dst`? No.
--                 For this, do I need the word-`dst`? No.
--                 So I can go back a level and check `dst` there.

--              - if the word doesn't terminate here; and if the terminating
--                transition is the same as my character; then I connect the
--                word's `src` to the graph `dst` by using a NON-FINAL transition.
--                This gives me two input edges into that node, which is fine.
--       -}
--         else if anEdgeTerminatesAt src dst then
--           if edge_to_match.isFinalTransition then
--             if (src, dst) |> edgeContains edge_to_match.char then
--             else
--               if prefixMatches edge_to_match.src src then
--                 StopMatchingAnd <| ConnectOneBack
--                   (\transition_, (src_, dst_) ->
--                     { src = transition_.src
--                     , 

--       {-
--         3. I run out of characters, e.g. create-ate, but I am NOT at the first
--            vertex.  Note that I am NOT following the word-path to get here!
--            So, I can just join up the starting-vertex of the DAWG to the vertex
--            after this one, using the first character of the word as the
--            transition.

--         4. I'm at the first vertex, e.g. ate-create, but I haven't run out of
--            letters.  So, I should join from the current-edge letter, up to the
--            `dst` vertex.  If I joined up to the `src` vertex, I would create
--            a cycle, which is impermissible.

--         So much for the stop cases.  Now, the "complicated" cases:


--         Now, let us talk about going "backwards", i.e. going to start.  We
--         can call these _input_ paths, because the processing flows from them.

--         6. If there is one input-path and it contains the necessary character,
--            then we can remove the edge that we are on and continue backwards.
--            Even if we have to stop afterwards, this is certain.

--         7. If there is one input-path and it does not contain the correct
--            character, then we might be able to go back by ONE space—if we
--            do not have a terminal or split-forward just before—by adding to
--            the edgeSet.

--         8. If there are multiple input-paths, then we can pursue all of them,
--            and see which one gives us the best outcome (i.e. most consumed
--            nodes).
--       -}
--       -- let's try these one at a time.
--       -- we'll begin with the stuff we can determine immediately.
--       if isSplitForward src then
--         -- (1)
--         -- we're at a forward-split; we cannot proceed; we're done.
--         [(actions, count - 1)]
--       else if List.isEmpty remainingChars then
--         -- (3)
--         [(actions, count)] -- do nothing is the fittest option.
--       else
--         -- oh, *poot*.  We're going to have to do some work… but is it easy
--         -- work, at least?  Well, let's see.  Everything here is actually
--         -- about the TRANSITION back, and not about the 
--        if isConfluence dst then
--         -- Try (1)(a) first
--         case commonPathToStart remainingChars dst of
--           [] -> -- I could not find a common path, so (1)(a) is not applicable.
--             -- so, none of the 
--           firstInChain::_ ->            
--             Maybe.map2
--               (\edgeSet ch ->
--                 if Set.member (ch, 0) edgeSet then
--                   [(AddToExistingEdge { src = firstInChain, dst = dst, thisEdge = (ch, 0) } :: actions, count)]
--                 else
--                   [(AddToExistingEdge { src = firstInChain, dst = dst, thisEdge = (ch, 1) } :: actions, count)]
--               )
--               (Graph.getEdge firstInChain dst dawg.graph)
--               (List.head remainingChars)
--             -- for the 'default' to be true, something very weird must happen.
--             -- Either the edgeSet doesn't have the edge (although, in
--             -- `commonPathToStart` we check that it does have it!) or the
--             -- `remainingChars` list is empty (although, just above this,
--             -- we do an early exit if it is empty).  So it's pretty much
--             -- assured that something wEiRd must be going on if the 'default'
--             -- is selected here.
--             |> Maybe.withDefault []
--     -- follow back to obtain the maximum number of transition-actions.
--     -- We return a list of follow-back actions and the nodes "saved" in the
--     -- process, which is a measure of their goodness (higher is better).
--     followBack : List (UniqueIdentifier, SuffixMergeActions) -> List (SuffixMergeActions, Int)
--     followBack remainingVertices =
--       List.map (\startNode -> followBackFrom startNode [] 0 characters) remainingVertices

--   in
--     dawg

-- mergeSuffixes4 : DAWG -> DAWG
-- mergeSuffixes4 dawg =
--   dawg -- TODO.  See mergeSuffixes3.

-- mergeSuffixes3 : Char -> UniqueIdentifier -> Breadcrumbs -> DAWG -> DAWG
-- mergeSuffixes3 char lastVertexOfWord pathHere dawg =
--   let
--     lastVerticesCleaned =
--       -- these are the lastVertices, but without the lastVertexOfWord
--       Dict.update
--         char
--         (\set ->
--           Maybe.andThen
--             (\existing ->
--               Set.remove lastVertexOfWord existing
--               |> \cleaned ->
--                 if Set.isEmpty cleaned then
--                   Nothing
--                 else
--                   Just cleaned
--             ) set
--         )
--         dawg.lastVertices
--     possibleLeaves =
--       Dict.values lastVerticesCleaned
--       |> List.foldl Set.union Set.empty
--   in
--     case Dict.get char lastVerticesCleaned of
--       Nothing ->
--         -- there is no leaf vertex that ends with the same transition.
--         -- I must check all the other vertices to see if I can find
--         -- a prefix-match where I can add to the final edge-set.
--         mergeSuffixes4 dawg
--       Just sameCharLeafVertices ->
--         -- there is at least one other leaf vertex that ends with this
--         -- transition.  However, if that leaf vertex is the destination
--         -- of a confluence, then it may be that we cannot use it at all.
--         mergeSuffixes5 sameCharLeafVertices pathHere dawg 
      

-- mergeSuffixes2 : Breadcrumbs -> DAWG -> DAWG
-- mergeSuffixes2 pathHere dawg =
--   case pathHere of
--     [] -> dawg -- nothing to do!
--     (lastChar, lastVertex)::rest ->
--       mergeSuffixes3 lastChar lastVertex pathHere dawg

-- -- defaultTransition : NodeConnections
-- -- defaultTransition = Set.singleton ('❓', notFinalState)

-- {-| Merges suffixes of the graph.

-- `lastVertices` may or may not contain the breadcrumbs.
-- -}
-- mergeSuffixes : Breadcrumbs -> Graph UniqueIdentifier NodeConnections -> Dict Char (Set UniqueIdentifier) -> Graph UniqueIdentifier NodeConnections
-- mergeSuffixes pathHere graph lastVertices =
--   {- |Here we want to start at the set of leaf-common-vertices for this char.
--       Then move backwards.  At the very least, we should be able to merge the
--       final letter, right?  When we run out of matching vertices, then we're
--       done.

--       I should probably take into account the "final" transitions.  I'll
--       handle those when I get to it…
--   -}
--   let
--     reversed = Graph.reverseEdges (debugGraph "suffix-merging of" graph)
--     characters = charsOfBreadcrumbs (pathHere |> Debug.log "Breadcrumbs are")
--     pathVertices = List.map Tuple.second pathHere |> Set.fromList |> Debug.log "Received word-path vertices are"
--     commonPathToStartExists : List Char -> UniqueIdentifier -> Bool
--     commonPathToStartExists charList vertex =
--       case charList of
--         [] -> True |> Debug.log "Common path to start exists"
--         ch::rest ->
--           case Graph.outgoingEdgesWithData vertex reversed of
--             [] -> False |> Debug.log "No confluence; A"
--             [(v, edgeSet)] ->
--               if Set.member (ch, 0) edgeSet || Set.member (ch, 1) edgeSet then
--                 commonPathToStartExists rest v
--               else
--                 False |> Debug.log "No confluence; B"
--             paths ->
--               List.any (\(v_, _) -> commonPathToStartExists rest v_) paths
--     tryOnlyPossibleConfluence : Int -> List Char -> UniqueIdentifier -> UniqueIdentifier -> List (UniqueIdentifier, Int)
--     tryOnlyPossibleConfluence count charList dst src =
--       {- |We might be able to add this
--           transition to the existing edgeSet.  I can do that if
--           TWO conditions are met:

--           1. Just beyond this vertex, there MUST be a common path
--               to the start.
--           2. The destination vertex cannot be a vertex that splits.
--               (we have already covered the case of a confluence, so
--               we do not need to think about it now; but it is the
--               same sort of reasoning that forbids a splitting
--               destination.)
        
--           However, we can only add ONE such transition for this
--           entire word, because AFTER we have added such a
--           transition, we WILL have a confluence.

--           So, let us see if these two conditions are met!
--       -}
--       if commonPathToStartExists charList src then -- && List.length (Graph.outgoingEdges dst graph) == 1 then
--         -- we can add this transition.
--         [(dst, 1 + count)] |> Debug.log "CAN add confluence.  Doing it"
--       else
--         -- oh well, we tried.
--         [(src, count)] |> Debug.log "Can't add confluence; oh well, we tried"

--     -- returns the node to join to; the number of characters "saved" (which
--     -- will be taken off from the existing path); and whether the transition
--     -- is final.
--     maxPath : List Char -> UniqueIdentifier -> Int -> List (UniqueIdentifier, Int)
--     maxPath remaining vertex count =
--       -- stop conditions:
--       -- 1. I hit a confluence.
--       -- 2. I hit a terminal transition.
--       -- 3. I run out of transitions.
--       case Debug.log ("(maxPath, vertex id: " ++ String.fromInt vertex ++ ") examining") remaining of
--         [] -> [(vertex, count)] |> Debug.log "I should not be here."
--         ch::rest ->
--           -- find the edge(s) that lead "away" from here (i.e. to src)
--           case Graph.outgoingEdgesWithData vertex reversed of
--             [] -> [(0, count)] |> Debug.log "Ended at the start"
--             [(v, edgeSet)] -> -- there is only one `src` node
--               if Set.size edgeSet == 1 then -- yay, only one path back
--                 if Set.member (ch, 0) edgeSet then
--                   -- keep going.
--                   maxPath rest v (1+count)
--                 else if Set.member (ch, 1) edgeSet then
--                   -- I cannot proceed past a terminal, because if I do, then
--                   -- I permit an extra word (or more) to be formed.
--                   [(vertex, count)]
--                   |> Debug.log ("There is a terminal at " ++ String.fromInt v ++ " that I can't proceed past")
--                 else
--                   -- no transitions from here.  However, we have not yet
--                   -- reached a confluence.                    -}
--                   tryOnlyPossibleConfluence count rest vertex v
--                   |> Debug.log "No transitions from here; but we see if we can find a confluence"
--               else -- more than one path back; i.e. confluence.
--                 -- we MUST stop here, in any case.
--                 -- however, the same argument applies as above for this.
--                 -- I can play the trick just once, IF it's applicable!
--                 tryOnlyPossibleConfluence count rest vertex v
--                 |> Debug.log "Must stop at confluence, but can we add to it?"                
--             pathSet ->
--               -- more than one path back.  HOWEVER, if one of them has a single
--               -- edge that leads away, AND it matches my character, then let's
--               -- go for it.
--               List.filter (\(_, edgeSet) -> Set.size edgeSet == 1 && Set.member (ch, 0) edgeSet) pathSet
--               |> List.concatMap (\(v, _) -> maxPath rest v (1 + count))
--     mostLikelyPath : Maybe (UniqueIdentifier, Int)
--     mostLikelyPath =
--       List.head characters
--       |> Maybe.andThen (\char -> Dict.get char lastVertices)
--       |> Maybe.andThen
--         (\vertexSet ->
--             Set.map (\v -> maxPath characters v 0) vertexSet
--             |> Set.toList
--             |> List.concatMap identity
--             -- if the path-length is zero, no merging is possible.
--             -- if the vertex is 0, we can't do anything with the start-vertex.
--             -- if the vertex is already on the path, then we're looking
--             -- at the exact word; we can't merge into ourselves, so ignore it.
--             |> List.filter (\(v, n) -> n > 0 && v /= 0 && not (Set.member v pathVertices))
--             |> List.maximum
--         )
--       |> Debug.log "Most likely path"
--     longestPath : Maybe (UniqueIdentifier, Int)
--     longestPath =
--       mostLikelyPath
--       |> Maybe.orElseLazy
--         (\() ->
--           -- starting from the correct character didn't work.
--           -- that's fine; let's start from anywhere else and
--           -- see if we can merge there.
--           lastVertices
--           |> Dict.toList
--           |> List.filterMap
--             -- this lambda will get ALL the possibilities.
--             (\(_, vertexSet) ->
--               Set.map (\v -> maxPath characters v 0) vertexSet
--               |> Set.toList
--               |> List.concatMap identity
--               -- if the path-length is zero, no merging is possible.
--               -- if the path-length is the same as pathHere, then we're looking
--               -- at the exact word; we can't merge into ourselves, so ignore it.
--               |> List.filter (\(_, n) -> n > 0) -- && n < List.length pathHere)
--               |> List.maximum
--             )
--           |> Debug.log "Extended the possibilities for path"
--           |> List.maximumBy Tuple.second
--         )
--       |> Debug.log "longest path"
--     -- contains (those to remove, those remaining)
--     edgesToRemove =
--       Maybe.map
--         (\(_, n) -> List.take n (breadcrumbsToForwardEdges pathHere |> Debug.log "Converting breadcrumbs to forward edges"))
--         longestPath |> Debug.log "Edges to remove"
--     -- this is the crumb of the original to join from.
--     joiningCrumb : Maybe (Char, UniqueIdentifier) -- isFinal
--     joiningCrumb =
--       Maybe.andThen
--         (\(_, n) ->
--           case List.drop n pathHere of
--             [] -> -- must be all the way at the start!
--               List.last pathHere -- if this doesn't exist, something big has gone wrong!
--             x::_ ->
--               Just x
--         )
--         longestPath
--       |> Debug.log "Joining at"
--     sansEdges : Maybe (Graph UniqueIdentifier NodeConnections)
--     sansEdges =
--       Maybe.map
--         (List.foldl
--           (\(a, b) g ->
--             Graph.removeEdge a b g
--             |> Graph.removeVertex b
--             |> debugGraph ("Removing edge " ++ String.fromInt a ++ "-" ++ String.fromInt b ++ " & vertex")
--           )
--           graph
--         )
--         edgesToRemove
--     redirectedGraph : Maybe (Graph UniqueIdentifier NodeConnections)
--     redirectedGraph =
--       Maybe.map3
--         (\(ch, src) (dst, _) g ->
--           case Graph.getEdge src dst g of
--             Nothing ->
--               -- if there is no src-dst edge, add it.
--               Graph.addEdge src dst (Set.singleton (ch, 0)) g
--               |> debugGraph ("Adding new edge '" ++ String.fromChar ch ++ "', " ++ String.fromInt src ++ "-" ++ String.fromInt dst)
--             Just edgeSet ->
--               Graph.updateEdge src dst (\_ -> Set.insert (ch, 0) (Debug.log "Found existing edgeSet" edgeSet)) g
--               |> debugGraph ("Updating edgeSet with new edge '" ++ String.fromChar ch ++ "', " ++ String.fromInt src ++ "-" ++ String.fromInt dst)
--         )
--         joiningCrumb longestPath sansEdges
--   in
--     Debug.log "\n\n" ()
--     |> \_ -> redirectedGraph |> Maybe.withDefault graph

mergeSuffixes : Breadcrumbs -> Graph UniqueIdentifier NodeConnections -> Dict Char (Set UniqueIdentifier) -> Graph UniqueIdentifier NodeConnections
mergeSuffixes pathHere graph lastVertices =
  graph


{-| Add a new vertex to lastVertices set. Only does this if the vertex is a leaf.
-}
addVertexToLastVertices : Char -> UniqueIdentifier -> Graph UniqueIdentifier NodeConnections -> Dict Char (Set UniqueIdentifier) -> Dict Char (Set UniqueIdentifier)
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

updateExistingEdges : Breadcrumbs -> Char -> Bool -> NodeConnections -> UniqueIdentifier -> UniqueIdentifier -> DAWG -> (DAWG, UniqueIdentifier)
updateExistingEdges pathHere char isFinal edgeSet src dst dawg =
  if isFinal then
    let
      -- update the graph to reflect that
      graph =
        -- if there is an identical non-terminal transition, remove it before
        -- adding the terminal transition.
        Graph.updateEdge src dst (\_ -> Set.remove (char, 0) edgeSet |> Set.insert (char, 1)) dawg.graph
      -- add this to the lastVertices, using the char as the key, IF
      -- there are no outgoing edges from here.  And there may be; consider
      -- freddy-fred-frederick, where we end on the first 'd' as well,
      -- but we extend past it to the two others.  In that case, the 'd'
      -- is a termination transition, but not a genuine leaf.
      lastVertices =
        addVertexToLastVertices char dst dawg.graph dawg.lastVertices
      -- if there are no outgoing edges from here, then we can also
      -- attempt a merge
      merged =
        if Graph.outgoingEdges dst dawg.graph == [] then
          mergeSuffixes ((char, dst)::pathHere) graph lastVertices
        else
          graph
    in
      ( { dawg | graph = merged, lastVertices = lastVertices }
      , dst
      )
  else
    ( dawg, dst )

{-| Given a starting vertex, add a new edge & vertex to the DAWG.  Returns the updated DAWG and the new vertex.

If the starting vertex is in the lastVertices set, then it is removed; it is no longer a leaf.  This does
not change its status as a final vertex.
-}
addNewEdge : Char -> Bool -> Breadcrumbs -> UniqueIdentifier -> DAWG -> (DAWG, UniqueIdentifier)
addNewEdge char isFinal pathHere vertex dawg =
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
    lastVerticesCleaned =
      -- a fair bit of code to basically remove non-leaf vertices, brute-force style
      Dict.keys dawg.lastVertices
      |> List.foldl
        (\k d ->
          Dict.update k
            (\v ->
              Maybe.andThen
                (\s ->
                  let
                    s_ = Set.remove vertex s
                  in
                    if Set.isEmpty s_ then
                      Nothing
                    else
                      Just s_
                )
                v
            )
            d
        )
        dawg.lastVertices
    lastVertices =
      if isFinal then
        addVertexToLastVertices char dawg.nextId graph lastVerticesCleaned
      else
        lastVerticesCleaned
    merged =
      if isFinal then
        mergeSuffixes ((char, dawg.nextId)::pathHere) graph lastVertices
      else
        graph    
    nextId = dawg.nextId + 1
  in
    ( { dawg
      | graph = merged
      , nextId = nextId
      , lastVertices = lastVertices
      }
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
      addNewEdge char isFinal pathHere vertex dawg
    -- (2) If we already have this edge, then we're good.
    else
      case existingEndVertex of
        Just (endVertex, edgeSet) ->
          -- nothing to do; this is already here.
          -- BUT, if the edge is specified to be final, then we should
          -- update the existing edge-set.
          updateExistingEdges pathHere char isFinal edgeSet vertex endVertex dawg
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

                  (ii) we can split to a new edge.  This is the default
                       behaviour because of what is mentioned in (i).

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
                        annoying problems of "which vertices are final?".

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
                  (\ch (graph, pathVertex) -> addNewEdge ch False pathHere pathVertex graph)
                  (dawg, confluenceVertex)
                  to_build
              Nothing ->
                -- we are in the situation of (b)(i): there is no confluence.
                addNewEdge char isFinal pathHere vertex dawg
          else
            -- this is the situation for (a)(i) and (a)(ii)
            addNewEdge char isFinal pathHere vertex dawg


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
                (dawg_, node_) = addTransition node ch False crumbs g
              in
                proceed ((ch, node_)::crumbs) node_ dawg_ rest
      in
        proceed [] dawg.firstVertex dawg characters

fromWords : List String -> DAWG
fromWords =
  List.foldl addString empty

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  Debug.log txt
    { graph = Graph.verticesAndEdges dawg.graph
    , firstVertex = dawg.firstVertex
    , lastVertices = dawg.lastVertices
    , nextId = dawg.nextId
    }
  |> \_ -> dawg

debugGraph : String -> Graph UniqueIdentifier NodeConnections -> Graph UniqueIdentifier NodeConnections
debugGraph txt graph =
  Debug.log txt
    (Graph.verticesAndEdges graph)
  |> \_ -> graph

numNodes : DAWG -> Int
numNodes dawg =
  List.length <| Graph.vertices dawg.graph

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
    processTransition : UniqueIdentifier -> List Char -> UniqueIdentifier -> TransitionType -> (List (String, UniqueIdentifier))
    processTransition started_at seen dst transition =
      case transition of
        SameStream (ch, nextVertex) ->
          fromRightToLeft started_at (ch::seen) nextVertex
        AdditionalStart (ch, nextVertex) ->
          -- check: is this a leaf?  If not, then kick off a new one.
          case Graph.outgoingEdges dst dawg.graph of
            [] -> fromRightToLeft started_at (ch::seen) nextVertex -- we are a leaf.
            _ -> fromRightToLeft started_at (ch::seen) nextVertex ++ fromRightToLeft dst [ch] nextVertex
    -- we might have arrived at a particular final-node via two paths,
    -- totally independently (e.g. fred-freddy-frederick), and we have
    -- no idea whether we've seen this final-node or not.  So, we track
    -- the started_at vertex—which is the final-vertex of a word—, and
    -- we remove duplicates using it.
    fromRightToLeft : UniqueIdentifier -> List Char -> UniqueIdentifier -> (List (String, UniqueIdentifier))
    fromRightToLeft started_at seen vertex =
      case analyzeVertex vertex of
        End -> [(String.fromList seen, started_at)]
        SinglePath v ->
          processTransition started_at seen vertex v
        MultiplePaths transitions ->
          List.concatMap (processTransition started_at seen vertex) transitions
  in
    dawg.lastVertices
    |> Dict.values
    |> List.concatMap
        (\v ->
          List.concatMap
            (\lastVertex -> fromRightToLeft lastVertex [] lastVertex)
            (Set.toList v)
        )
    -- here's where we remove duplicates obtained via independent paths.
    |> List.uniqueBy Tuple.second
    |> List.map Tuple.first
    |> List.sort

{-
{-| Change the transition-value between nodes.
-}
modifyTransition : UniqueIdentifier -> UniqueIdentifier -> NodeConnections -> DAWG -> DAWG
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