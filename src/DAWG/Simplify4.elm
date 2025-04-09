module DAWG.Simplify4 exposing (..)
import DAWG.Data exposing (DAWG)
import Tree.Diff exposing (Tail(..))
import Dict exposing (Dict(..))
import Set exposing (Set)
import DAWG.Data exposing (ExprAST(..))
import List.Extra

type CustomTree a
  = LeftRight
      (Dict a (Bool, CustomTree a)) -- "Dict a" gives you the left-right, CustomTree gives you the depth, the Bool gives you whether it's terminal.

type alias Formulaic a =
  { tree : CustomTree a
  , pathsToEnd : Dict a (Int, Set (List a))  -- map of ending-characters (final, not terminal) to (count-of-occurrences, paths-to-end)
  }

type UpDownAxis a
  = UpDown (CustomTree a) (List (a, CustomTree a)) -- root, path

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
      if List.isEmpty pathFollowed then
        NewEntryIsUnrelated -- and actually, new entry is… an empty string!?
      else
        -- I did follow some path to get here.
        -- And there is still more path ahead.
        -- But the new entry ends here. Therefore…
        NewEntryIsPrefix
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

navigate : CustomTree comparable -> UpDownAxis comparable
navigate root =
  UpDown root []

up : UpDownAxis comparable -> UpDownAxis comparable
up (UpDown root path) =
  case path of
    [] ->
      UpDown root path
    _::t ->
      UpDown root t

down : comparable -> UpDownAxis comparable -> UpDownAxis comparable
down key ((UpDown (LeftRight rootDict as root) path) as nav) =
  let
    search_dict =
      case path of
        (_, LeftRight result)::_ -> result
        _ -> rootDict
  in
    Dict.get key search_dict
    |> Maybe.map (\(_, lr_result) -> UpDown root ((key, lr_result) :: path))
    |> Maybe.withDefault nav

get : UpDownAxis comparable -> Maybe comparable
get (UpDown _ path) =
  case path of
    [] ->
      Nothing
    (k, _)::_ ->
      Just k

siblings : UpDownAxis comparable -> List comparable
siblings (UpDown (LeftRight rootDict) path) =
  case path of
    [] ->
      Dict.keys rootDict
    (_, LeftRight lastResult)::_ ->
      Dict.keys lastResult


















-- NONSENSE (for now)
toDAWG : List String -> DAWG
toDAWG xs =
  xs |> List.map String.toList
  |> List.foldl insert emptyFormula
  |> \_ -> DAWG.Data.empty