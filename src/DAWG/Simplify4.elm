module DAWG.Simplify4 exposing (..)
import DAWG.Data exposing (DAWG)
import Tree.Diff exposing (Tail(..))
import Dict exposing (Dict(..))
import IntDict exposing (IntDict)
import Dict exposing (update)

type CustomTree a
  = LeftRight (Dict a (Bool, CustomTree a)) -- "Dict a" gives you the left-right, CustomTree gives you the depth, the Bool gives you whether it's terminal.

type UpDownAxis a
  = UpDown (CustomTree a) (List (a, CustomTree a)) -- root, up, down

empty : CustomTree a
empty = LeftRight Dict.empty

insertDown : List comparable -> CustomTree comparable -> CustomTree comparable
insertDown cx (LeftRight dict) =
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
                Just (False, insertDown rest empty) -- |> Debug.log "Inserted new"
              Just (isT, sublevel) ->
                -- this already exists, so just follow it.
                Just <| (isT, insertDown rest sublevel) -- |> Debug.log "Already exists, following"
          )
          dict

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
toDAWG _ =
  DAWG.Data.empty