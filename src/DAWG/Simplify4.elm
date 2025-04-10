module DAWG.Simplify4 exposing (..)
import DAWG.Data exposing (DAWG)
import Tree.Diff exposing (Tail(..))
import Dict exposing (Dict(..))
import Set exposing (Set)
import Array
import Cone3d exposing (along)
-- import List.Extra

type CustomTree a
  = LeftRight
      (Dict a (Bool, CustomTree a)) -- "Dict a" gives you the left-right, CustomTree gives you the depth, the Bool gives you whether it's terminal.

type alias Formulaic a =
  { tree : CustomTree a
  , pathsToEnd : Dict a (Int, Set (List a))  -- map of ending-characters (final, not terminal) to (count-of-occurrences, paths-to-end)
  }

type UpDownAxis a
  = UpDown (CustomTree a) (List (a, CustomTree a)) -- root, path

type alias Formulaic2 a =
  { tree : CustomTree a
  , zippersToEnd : Dict a (List (UpDownAxis a)) -- this just gives the "final" similarities.
  }

empty : CustomTree a
empty = LeftRight Dict.empty

emptyFormula : Formulaic a
emptyFormula =
  { tree = empty
  , pathsToEnd = Dict.empty
  }

insert_ : List comparable -> CustomTree comparable -> CustomTree comparable
insert_ cx (LeftRight dict) =
  case cx of
    [] -> LeftRight dict
    [c] ->
      LeftRight <|
        Dict.update c
          (\node ->
            case node of
              Nothing -> -- then, I should insert it here.
                Just (True, empty) -- |> Debug.log "Adding final (new) item"
              Just (_, existing) ->
                -- aand, we're done.
                Just (True, existing) -- |> Debug.log "Final item already exists"
          )
          dict
    c::rest ->
      LeftRight <|
        Dict.update c
          (\node ->
            case node of
              Nothing -> -- then, I should insert it here.
                -- this is a new path, and it's continuing, so this isn't terminal.
                Just (False, insert_ rest empty) -- |> Debug.log "Inserted new"
              Just (isT, sublevel) ->
                -- this already exists, so just follow it.
                Just <| (isT, insert_ rest sublevel) -- |> Debug.log "Already exists, following"
          )
          dict

type InsertionConclusion a
  = NewEntryIsPrefix
  | NewEntryIsExtension (List a) -- the prefix that is being extended
  | NewEntryIsUnrelated
  | NewEntryIsDuplicate

checkNewEntry : List comparable -> CustomTree comparable -> List comparable -> InsertionConclusion comparable
checkNewEntry cx (LeftRight dict) pathFollowed =
  case cx of
    [] ->
      -- if List.isEmpty pathFollowed then
        -- new entry is actually an empty string, it seems.
        NewEntryIsUnrelated -- and actually, new entry is… an empty string!?
      -- else -- I should never be able to reach this, UNLESS I get an empty string.
        -- I did follow some path to get here.
        -- And there is still more path ahead.
        -- But the new entry ends here. Therefore…
        -- NewEntryIsPrefix
    [c] -> -- this is the end of the word.
      case Dict.get c dict of
        Nothing ->
          -- So, I got here, but now—right at the end—there is no continuation.
          -- This is irrespective of whether we followed a path or not, or
          -- whether the present dict is empty or not.
          NewEntryIsUnrelated
        Just (True, d) ->
          -- whether the dictionary is empty or not, the path ends here.
          -- So, this is a duplicate of some other seen word.
          NewEntryIsDuplicate
        Just (False, d) ->
          NewEntryIsPrefix -- actually, it's all here, just non-terminal!!
    c::rest ->
      case Dict.get c dict of
        Nothing ->
          if List.isEmpty pathFollowed then
            NewEntryIsUnrelated
          else if Dict.isEmpty dict then
            -- the new entry goes past the old stuff.
            NewEntryIsExtension <| List.reverse (c::pathFollowed)
          else
            NewEntryIsUnrelated
        Just (_, LeftRight d as subtree) ->
          if Dict.isEmpty d then
            -- the path stops here, but we carry on.
            NewEntryIsExtension <| List.reverse (c::pathFollowed)
          else
            checkNewEntry rest subtree (c::pathFollowed)

insert : List comparable -> Formulaic comparable -> Formulaic comparable
insert cx {tree, pathsToEnd} =
  -- With regard to tracking the paths to the end, there are 4 possibilities
  -- when I get an entry.
  -- 1. The new entry is a prefix of an existing entry.
  -- 2. The new entry is an extension of an existing entry.
  -- 3. The new entry is unrelated to an existing entry.
  -- 4. The new entry is exactly the same as an existing entry.

  {- So, let me deal with these three cases.

  1. If I go through the existing tree, I can tell if it's a prefix.
  2. If I go through the existing tree, I can tell if it's an extension.
  3. If I go through the existing tree, I can tell if it's unrelated.
  -}
  let
    addOrUpdatePath : Dict comparable (Int, Set (List comparable)) -> Dict comparable (Int, Set (List comparable))
    addOrUpdatePath paths =
      List.reverse cx
      |> (\cx_rev ->
        case cx_rev of
          [] -> paths
          last::rest ->
            Dict.update last
              (\v ->
                case v of
                  Nothing ->
                    Just (1, Set.singleton <| List.reverse rest)
                  Just (n, existing) ->
                    Just ( n + 1, Set.insert (List.reverse rest) existing )
              )
              paths
      )
    removePath : List comparable -> Dict comparable (Int, Set (List comparable)) -> Dict comparable (Int, Set (List comparable))
    removePath path paths =
      case List.reverse path of
        [] ->
          paths
        last::rest ->
          Dict.update last
            (Maybe.andThen (\(n, entry) ->
              if n == 1 then
                Nothing
              else
                Just <| ( n - 1, Set.remove rest entry )
            ))
            paths
  in
  { tree = insert_ cx tree
  , pathsToEnd =
      case checkNewEntry cx tree [] of
        NewEntryIsPrefix ->
          pathsToEnd
        NewEntryIsDuplicate ->
          pathsToEnd
        NewEntryIsUnrelated ->
          addOrUpdatePath pathsToEnd
        NewEntryIsExtension old ->
          addOrUpdatePath pathsToEnd
          |> removePath old
  }


binarySearch : comparable -> Array.Array comparable -> Maybe Int
-- Array.get is O(log_{32} n) so this is (access-cost × search-cost) = ((log_{32} n) × (log_2 n))
binarySearch target array =
  let
    size =
      Array.length array
  in
    if size == 0 then
      Nothing
    else
      let
        helper low high =
          if low > high then
            Nothing
          else
            let
              mid = (low + high) // 2
              midVal = Array.get mid array
            in
              midVal
              |> Maybe.andThen
                (\val ->
                  if val == target then
                    Just mid
                  else if val < target then
                    helper (mid + 1) high
                  else
                    helper low (mid - 1)
                )
      in
        helper 0 (size - 1)


navigate : Formulaic comparable -> UpDownAxis comparable
navigate {tree} =
  UpDown tree []

up : UpDownAxis comparable -> Maybe (UpDownAxis comparable)
-- O(1)
up (UpDown root path) =
  case path of
    [] ->
      Nothing
    _::t ->
      Just <| UpDown root t

thenUp : Maybe (UpDownAxis comparable) -> Maybe (UpDownAxis comparable)
-- O(1)
thenUp = Maybe.andThen up

down : comparable -> UpDownAxis comparable -> Maybe (UpDownAxis comparable)
-- O(log_2 n)
down key ((UpDown (LeftRight rootDict as root) path)) =
  let
    search_dict =
      case path of
        (_, LeftRight result)::_ -> result
        _ -> rootDict
  in
    Dict.get key search_dict
    |> Maybe.map (\(_, lr_result) -> UpDown root ((key, lr_result) :: path))

thenGo : (UpDownAxis comparable -> Maybe (UpDownAxis comparable)) -> Maybe (UpDownAxis comparable) -> Maybe (UpDownAxis comparable)
-- O(1)
thenGo nav v =
  Maybe.andThen nav v

get : UpDownAxis comparable -> Maybe comparable
-- O(1)
get (UpDown _ path) =
  case path of
    [] ->
      Nothing
    (k, _)::_ ->
      Just k

getTransition : UpDownAxis comparable -> Maybe (comparable, Terminality)
getTransition ((UpDown (LeftRight rootDict) _) as nav) =
  let
    search_dict (UpDown _ upPath) =
      case upPath of
        (_, LeftRight result)::_ -> result
        _ -> rootDict
  in
  get nav
  |> Maybe.andThen
    (\key ->
      up nav
      |> Maybe.map search_dict
      |> Maybe.withDefault rootDict
      |> Dict.get key
      |> Maybe.map (\(finality, _) ->
          ( key, if finality then Terminal else NotTerminal )
        )
    )

thenGetTransition : Maybe (UpDownAxis comparable) -> Maybe (comparable, Terminality)
thenGetTransition = Maybe.andThen getTransition

thenGet : Maybe (UpDownAxis comparable) -> Maybe comparable
-- same as get(…)
thenGet = Maybe.andThen get

alongPath : List comparable -> UpDownAxis comparable -> Maybe (UpDownAxis comparable)
-- This invokes `down` (O(log_2 n)) /m/ times, where /m/ is |list|
-- So, O(m log_2 n)
alongPath directions updown =
  List.foldl (\dir -> thenGo (down dir)) (Just updown) directions

children : UpDownAxis comparable -> List comparable
-- O(log_2 n)
children (UpDown (LeftRight rootDict) path) =
  case path of
    [] ->
      Dict.keys rootDict
    (_, LeftRight lastResult)::_ ->
      Dict.keys lastResult

thenGetChildren : Maybe (UpDownAxis comparable) -> Maybe (List comparable)
-- same Big-O as `children`
thenGetChildren = Maybe.map children

siblings : UpDownAxis comparable -> Maybe (List comparable)
-- O(log_2 n)
siblings =
  up >> thenGetChildren

thenGetSiblings : Maybe (UpDownAxis comparable) -> Maybe (List comparable)
-- Same Big-O as `children`; `up` is O(1)
thenGetSiblings = Maybe.andThen siblings

first : UpDownAxis comparable -> Maybe (UpDownAxis comparable)
-- O(2 * log_2 n)
first updown =
  let
    upper = up updown -- O(1)
    sibs = thenGetChildren upper -- O(log_2 n)
    firstSibling = Maybe.andThen List.head sibs -- O(1)
  in
    -- `down` is O(log_2 n)
    Maybe.andThen (\key -> thenGo (down key) upper) firstSibling

thenFirst : Maybe (UpDownAxis comparable) -> Maybe (UpDownAxis comparable)
thenFirst = Maybe.andThen first

right : UpDownAxis comparable -> Maybe (UpDownAxis comparable)
-- O(1) + O(log_2 n) + ≈O(log_2 n) + O(1) + ≈O(1) + ≈O(log_2 n) = O(≈3 + 3 log_2 n)
right nav =
  let
    key = get nav -- O(1)
    sibs = siblings nav |> Maybe.map Array.fromList -- O(log_2 n)
    keyIndex = Maybe.andThen (\k -> Maybe.andThen (binarySearch k) sibs) key -- O((log_2 n) * (log_32 n)) … but (log_32 n) is pretty close to O(1) for a latin alphabet!
    nextKeyIndex = Maybe.map ((+) 1) keyIndex -- O(1)
    nextKey = Maybe.andThen (\sibs_ -> Maybe.andThen (\i -> Array.get i sibs_) nextKeyIndex) sibs -- O(log_32 n)
    rightAxis = Maybe.andThen (\k -> up nav |> thenGo (down k)) nextKey -- O(log_2 n)
  in
    rightAxis

thenRight : Maybe (UpDownAxis comparable) -> Maybe (UpDownAxis comparable)
thenRight = Maybe.andThen right

formulaic_to_formulaic2 : Formulaic comparable -> Formulaic2 comparable
-- Pretty darn expensive.
-- O(e * (log_2 p + p * m log_2 n)), where
-- /e/ = number of unique final chars,
-- /p/ = combined size of all path-sets, i.e. total number of paths through the tree
-- /m/ = size of paths
-- /n/ = |Σ|
formulaic_to_formulaic2 input =
{-
type alias Formulaic a =
  { tree : CustomTree a
  , pathsToEnd : Dict a (Int, Set (List a))  -- map of ending-characters (final, not terminal) to (count-of-occurrences, paths-to-end)
  }

to

type alias Formulaic2 a =
  { tree : CustomTree a
  , zippersToEnd : Dict a (List (UpDownAxis a)) -- this just gives the "final" similarities.
  }
-}
  { tree = input.tree
  , zippersToEnd =
      Dict.map -- O(e) where /e/ = number of unique final chars
        (\_ (_, set) ->
          Set.toList set -- O(log_2 p) where /p/ = |set|
          |> List.foldl -- Total cost: O(p * m log_2 n), where /p/ = |set|
            (\path (acc, base) ->
              case alongPath path base of -- O(m log_2 n) where /m/ = |path| and /n/ = |Σ|
                Nothing ->
                  ( acc, base )
                Just v ->
                  ( v :: acc, base )
            )            
            ([], UpDown input.tree [])
          |> Tuple.first
        )
        input.pathsToEnd
  }

type Sharing
  = SharedLinks
  | NotShared

type Terminality
  = Terminal
  | NotTerminal

type alias Transition a = (a, Terminality)

type ExprAST a
  = M (List (ExprAST a))
  | A (List (ExprAST a), Sharing)
  | V (Transition a, Sharing)

-- recreateFormula : Formulaic a -> ExprAST a
-- recreateFormula formula =
--   -- Start from the ends.



























-- NONSENSE (for now)
toDAWG : List String -> DAWG
toDAWG xs =
  xs |> List.map String.toList -- trivial. ≈112k, 56.2k, 10.5k for 50, 100, 500 respectively.
  |> List.foldl insert emptyFormula -- ≈6.1k, 2.7k, 410 for 50, 100, 500 respectively.  This is WITH updating "paths" during.
  |> formulaic_to_formulaic2 -- ≈3.7k, 1.7k, 243 for 50, 100, 500 respectively.  That's actually not as horrific as I was expecting, but still pretty bad.
  |> \_ -> DAWG.Data.empty