module Main exposing (..)

import Browser
import Browser.Events as BE
import Html.Styled exposing (Html, div, h3, p, ul, li, input, textarea, span, toUnstyled, text, button, strong)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph as FDG
import Automata.Data exposing (..)
import Browser.Dom
import Task
import Platform.Cmd as Cmd
import Html.Styled.Attributes as HA
import List.Extra
import Automata.DFA
import Html
import Maybe.Extra
import Css
import Platform.Cmd as Cmd
import Uuid
import Random.Pcg.Extended as Random
import Time
import Dict exposing (Dict)
import Ports
import Platform.Cmd as Cmd
import Automata.DFA as DFA
import Automata.Data as Data
import Graph exposing (NodeId, Edge)
import Result.Extra
import TypedSvg.Types exposing (Paint(..))
import Css exposing (rgba)
import TypedSvg exposing
  (circle, g, svg, title, text_, marker, path, defs, tspan, rect)
import TypedSvg.Attributes exposing
  ( class, fill, stroke, viewBox, fontFamily, fontWeight, alignmentBaseline
  , textAnchor, cursor, id, refX, refY, orient, d, markerEnd, dominantBaseline
  , transform, noFill, strokeDasharray, strokeLinecap, cursor
  , markerStart, pointerEvents, dy)
import TypedSvg.Attributes.InPx exposing
  ( cx, cy, r, strokeWidth, x, y, height, fontSize
  , markerWidth, markerHeight, width, rx , ry)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import Color
import Html.Attributes
import Force
import Set exposing (Set)
import Svg.Attributes exposing (pointerEvents)

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type LeftPanelIcon
  = ComputationsIcon
  | SearchIcon
  | GitIcon
  | TestsIcon
  | ExtensionsIcon

type ExecutionState
  = Ready
  | NotReady
  | ExecutionComplete
  | StepThrough

type BottomPanel
  = AddTestPanel
  | EditDescriptionPanel

type alias GraphPackage =
  { model : FDG.Model
  , description : Maybe String
  , uuid : Uuid.Uuid
  , created : Time.Posix -- for ordering
  , currentTestKey : String
  , tests : Dict String String
  }

type alias Model =
  { currentPackage : GraphPackage
  , packages : Dict String GraphPackage
  , mainPanelDimensions : ( Float, Float )
  , leftPanelOpen : Bool
  , selectedIcon : Maybe LeftPanelIcon
  , leftPanelWidth : Float
  , rightBottomPanelOpen : Bool
  , rightBottomPanelHeight : Float
  , rightTopPanelDimensions : ( Float, Float )
  , isDraggingHorizontalSplitter : Bool
  , isDraggingVerticalSplitter : Bool
  , mousePosition : ( Float, Float )
  , executionState : ExecutionState
  , selectedBottomPanel : BottomPanel
  , uuidSeed : Random.Seed
  , currentTime : Time.Posix
  }

type alias Flags =
  { width : Float
  , height : Float
  , initialSeed : Int
  , extendedSeeds : List Int
  , startTime : Time.Posix
  , packages : List GraphPackage
  }

encodeGraphPackage : GraphPackage -> E.Value
encodeGraphPackage pkg =
  E.object
    [ ("model", DFA.serializeAutomatonGraph pkg.model.userGraph |> E.string)
    , ("description", Maybe.map E.string pkg.description |> Maybe.withDefault E.null)
    , ("uuid", Uuid.encode pkg.uuid)
    , ("created", E.int (Time.posixToMillis pkg.created))
    , ("tests", E.dict identity E.string pkg.tests)
    , ("currentTestKey", E.string pkg.currentTestKey)
    ]

decodeGraphPackage : (Float, Float) -> D.Decoder GraphPackage
decodeGraphPackage (w, h) =
  let
    initialize_fdg serialized =
      Data.mkAG_input serialized
      |> DFA.mkAutomatonGraph
      |> FDG.automatonGraphToModel (w, h)
  in
  D.map6 GraphPackage
    (D.field "model" <| D.map initialize_fdg D.string)
    (D.field "description" <| D.oneOf [ D.null Nothing, D.map Just D.string ])
    (D.field "uuid" Uuid.decoder)
    (D.field "created" <| D.map Time.millisToPosix D.int)
    (D.field "currentTestKey" D.string)
    (D.field "tests" <| D.dict D.string)

decodeFlags : D.Decoder Flags
decodeFlags =
  D.map2 (\w h -> (w, h))
    (D.field "width" D.float)
    (D.field "height" D.float)
  |> D.andThen
    (\(w, h) ->
      D.map4 (Flags w h)
        (D.field "initialSeed" D.int)
        (D.field "extendedSeeds" <| D.list D.int)
        (D.field "startTime" <| D.map Time.millisToPosix D.int)
        (D.field "packages" <| D.list (decodeGraphPackage (w, h)))
    )

createNewPackage : Uuid.Uuid -> Uuid.Uuid -> Time.Posix -> (Float, Float) -> GraphPackage
createNewPackage uuid testUuid currentTime (w, h) = -- this is the width & height of the panel
  { model = FDG.init ( w, h )
  , description = Nothing
  , uuid = uuid
  , created = currentTime
  , currentTestKey = Uuid.toString testUuid
  , tests = Dict.empty
  }

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    decoded =
      D.decodeValue decodeFlags flags
      |> Result.mapError (\err -> Debug.log "OH NO! FLAGS WAS NOT PARSED CORRECTLY!!" err)
      |> Result.withDefault (Flags 80 60 0 [] (Time.millisToPosix 0) [])
    
    initialRightTopWidth = decoded.width - 60  -- 60px for icon bar
    initialRightTopHeight = decoded.height - 223  -- 30px status bar + 185px bottom panel + 8px splitter
    initialSeed = Random.initialSeed decoded.initialSeed decoded.extendedSeeds
    (uuid, newSeed) = Random.step Uuid.generator initialSeed
    (uuid2, newSeed2) = Random.step Uuid.generator newSeed
  in
    ( { currentPackage = createNewPackage uuid uuid2 decoded.startTime (initialRightTopWidth, initialRightTopHeight)
      , packages =
          decoded.packages |> List.map (\v -> ( Uuid.toString v.uuid, v )) |> Dict.fromList
      , mainPanelDimensions = ( decoded.width, decoded.height )
      , leftPanelOpen = False
      , selectedIcon = Nothing
      , leftPanelWidth = 250
      , rightBottomPanelOpen = True
      , rightBottomPanelHeight = 185
      , rightTopPanelDimensions = ( initialRightTopWidth, initialRightTopHeight )
      , isDraggingHorizontalSplitter = False
      , isDraggingVerticalSplitter = False
      , mousePosition = ( 0, 0 )
      , executionState = Ready
      , selectedBottomPanel = AddTestPanel
      , uuidSeed = newSeed2
      , currentTime = decoded.startTime
      }
    , Cmd.none
    )

-- UPDATE

type Msg
  = ForceDirectedMsg FDG.Msg
  | OnResize (Float, Float)
  | SetMouseOver Bool
  | ClickIcon LeftPanelIcon
  | StartDraggingHorizontalSplitter
  | StartDraggingVerticalSplitter
  | StopDragging
  | MouseMove Float Float
  | ToggleBottomPanel
  | UpdateTestPanelContent String
  | UpdateDescriptionPanelContent String
  | UpdateRightTopDimensions Float Float
  | RunExecution
  | ResetExecution
  | StepThroughExecution
  | SelectBottomPanel BottomPanel
  | Seconded Time.Posix
  | CreateNewPackage
  | SelectPackage Uuid.Uuid
  | DeletePackage Uuid.Uuid
  | SelectTest String
  | DeleteTest String
  | CreateNewTest

{-| Note: EVERYWHERE that I use persistPackage, I should ALSO
    update the `packages` dictionary!

    (yes, I should one day figure out an automagic way to do this…
     maybe. but it's in so few places right now that hey, YAGNI?)
-}
persistPackage : GraphPackage -> Cmd Msg
persistPackage =
    Ports.saveToStorage << encodeGraphPackage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg {- |> (\v -> if v == ForceDirectedMsg FDG.Tick then v else Debug.log "MESSAGE" v) -} of
    ForceDirectedMsg fdMsg ->
      let
        currentPackage = model.currentPackage
        newFdModel =
          FDG.update (model.leftPanelWidth, 0) fdMsg currentPackage.model
          -- |> \v ->
          --   if fdMsg /= FDG.Tick then
          --     Debug.log "msg" fdMsg |> \_ ->
          --     FDG.debugModel_ "updated" v
          --   else
          --     v
        preUpdateChangeCount =
          List.length currentPackage.model.undoBuffer
        updatedPackage =
          { currentPackage | model = newFdModel }
      in
      ( { model
          | currentPackage = updatedPackage
          , executionState =
              if FDG.canExecute newFdModel then
                if model.executionState == NotReady then
                  Ready
                else
                  model.executionState
              else
                NotReady
          , packages =
              if preUpdateChangeCount > 0 && newFdModel.undoBuffer == [] then
                Dict.insert (Uuid.toString updatedPackage.uuid) updatedPackage model.packages
              else
                model.packages
        }
      , if preUpdateChangeCount > 0 && newFdModel.undoBuffer == [] then
          persistPackage updatedPackage
        else
          Cmd.none
      )

    OnResize (width, height) ->
      let
        newModel = { model | mainPanelDimensions = (width, height) }
        (newRightTopWidth, newRightTopHeight, newGraphPackage) = calculateRightTopDimensions newModel
      in
      ( { newModel
          | rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , currentPackage = newGraphPackage
        }
      , Cmd.none
      )

    SetMouseOver _ ->
      ( model, Cmd.none )

    ClickIcon icon ->
      let
        (newLeftPanelOpen, newSelectedIcon) =
          if model.selectedIcon == Just icon && model.leftPanelOpen then
            (False, Nothing)  -- Close panel if same icon clicked
          else
            (True, Just icon)  -- Open panel with new icon
        (newRightTopWidth, newRightTopHeight, newGraphPackage) = calculateRightTopDimensions { model | leftPanelOpen = newLeftPanelOpen }
      in
      ( { model 
        | leftPanelOpen = newLeftPanelOpen
        , selectedIcon = newSelectedIcon
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        , currentPackage = newGraphPackage
        }
      , Cmd.none
      )

    StartDraggingHorizontalSplitter ->
      ( { model | isDraggingHorizontalSplitter = True }, Cmd.none )

    StartDraggingVerticalSplitter ->
      ( { model | isDraggingVerticalSplitter = True }, Cmd.none )

    StopDragging ->
      ( { model 
        | isDraggingHorizontalSplitter = False
        , isDraggingVerticalSplitter = False
        }
      , Cmd.none
      )

    MouseMove x y ->
      let
        newModel = { model | mousePosition = (x, y) }
      in
      if model.isDraggingHorizontalSplitter then
        let
          (viewportWidth, _) = model.mainPanelDimensions
          minWidth = 100
          maxWidth = viewportWidth / 2
          newLeftPanelWidth = clamp minWidth maxWidth x
          (newRightTopWidth, newRightTopHeight, newGraphPackage) = calculateRightTopDimensions { newModel | leftPanelWidth = newLeftPanelWidth }
        in
        ( { newModel 
          | leftPanelWidth = newLeftPanelWidth
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , currentPackage = newGraphPackage
          }
        , Cmd.none
        )
      else if model.isDraggingVerticalSplitter then
        let
          (_, viewportHeight) = model.mainPanelDimensions
          minHeight = 185  -- 8em @ 16px font ≈ 128px
          maxHeight = viewportHeight / 2
          statusBarHeight = 30
          newBottomHeight = clamp minHeight maxHeight (viewportHeight - y - statusBarHeight)
          (newRightTopWidth, newRightTopHeight, newGraphPackage) = calculateRightTopDimensions { newModel | rightBottomPanelHeight = newBottomHeight }
        in
        ( { newModel
          | rightBottomPanelHeight = newBottomHeight
          , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , currentPackage = newGraphPackage
          }
        , Cmd.none
        )
      else
        ( newModel, Cmd.none )

    ToggleBottomPanel ->
      let
        newBottomPanelOpen = not model.rightBottomPanelOpen
        (newRightTopWidth, newRightTopHeight, newGraphPackage) = calculateRightTopDimensions { model | rightBottomPanelOpen = newBottomPanelOpen }
      in
      ( { model 
        | rightBottomPanelOpen = newBottomPanelOpen
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        , currentPackage = newGraphPackage
        }
      , Cmd.none
      )

    UpdateTestPanelContent content ->
      let
        currentPackage = model.currentPackage
        updatedTests =
          case content of
            "" ->
              Dict.remove currentPackage.currentTestKey currentPackage.tests
            _ ->
              Dict.insert currentPackage.currentTestKey content currentPackage.tests
        updatedPackage =
          { currentPackage | tests = updatedTests }
      in
        ( { model
            | currentPackage = updatedPackage
            , packages = Dict.insert (Uuid.toString currentPackage.uuid) currentPackage model.packages
          }
        , persistPackage updatedPackage
        )

    UpdateDescriptionPanelContent content ->
      let
        currentPackage = model.currentPackage
        updatedPackage =
          { currentPackage
            | description =
                if content == "" then
                  Nothing
                else
                  Just content
          }
      in
        ( { model
            | currentPackage = updatedPackage
            , packages = Dict.insert (Uuid.toString currentPackage.uuid) currentPackage model.packages
          }
        , persistPackage updatedPackage
        )

    UpdateRightTopDimensions width height ->
      ( { model | rightTopPanelDimensions = (width, height) }, Cmd.none )

    RunExecution ->
      -- for this and StepThrough, if the FDG.canExecute is false, then
      -- the 'Run' and 'Step' buttons on the UI will be disabled.
      -- Therefore, I don't need to handle those cases here, and can
      -- assume that when I get this message, the precondition of
      -- a 'Ready' state has already been met.
      ( { model 
        | executionState = ExecutionComplete
        , currentPackage =
            fdg_update model
              ( FDG.Load
                  ( Dict.get model.currentPackage.currentTestKey model.currentPackage.tests
                    |> Maybe.withDefault ""
                  )
              )
            |> \pkg -> fdg_update { model | currentPackage = pkg } FDG.Run
        }
      , Cmd.none
      )

    ResetExecution ->
      ( { model 
        | executionState = Ready
        , currentPackage = fdg_update model FDG.Stop
        }
      , Cmd.none
      )

    StepThroughExecution ->
      let
        newFdModel =
          if model.executionState /= StepThrough then
            -- this is the first click of stepping, so do a load first.
            fdg_update model
              ( FDG.Load
                  ( Dict.get model.currentPackage.currentTestKey model.currentPackage.tests
                    |> Maybe.withDefault ""
                  )
              )
          else
            fdg_update model FDG.Step
      in
      ( { model 
        | executionState =
            case newFdModel.model.execution of
              Nothing ->
                model.executionState
              Just (CanContinue _) ->
                StepThrough
              _ ->
                ExecutionComplete
        , currentPackage =
            newFdModel
        }
      , Cmd.none
      )

    SelectBottomPanel p ->
      ( { model | selectedBottomPanel = p }
      , Cmd.none
      )

    Seconded t ->
      ( { model | currentTime = t }
      , Cmd.none
      )

    CreateNewPackage ->
      let
        (uuid, newSeed) = Random.step Uuid.generator model.uuidSeed
        (uuid2, newSeed2) = Random.step Uuid.generator newSeed
      in
        ( { model
            | currentPackage = createNewPackage uuid uuid2 model.currentTime model.rightTopPanelDimensions
            , uuidSeed = newSeed2
          }
        , Cmd.none
        )

    SelectPackage uuid ->
      case Dict.get (Uuid.toString uuid) model.packages of
        Nothing ->
          ( model, Cmd.none )
        Just pkg ->
          ( { model | currentPackage = pkg }, Cmd.none )

    DeletePackage uuid ->
      ( { model | packages = Dict.remove (Uuid.toString uuid) model.packages }
      , Ports.deleteFromStorage (Uuid.toString uuid)
      )

    CreateNewTest ->
      let
        currentPackage = model.currentPackage
        (uuid, newSeed) = Random.step Uuid.generator model.uuidSeed
      in
        ( { model
            | currentPackage =
                { currentPackage | currentTestKey = Uuid.toString uuid }
            , uuidSeed = newSeed
          }
        , Cmd.none
        )

    SelectTest key ->
      let
        currentPackage = model.currentPackage
      in
        ( { model
            | currentPackage =
                { currentPackage | currentTestKey = key }
          }
        , Cmd.none
        )

    DeleteTest key ->
      let
        currentPackage = model.currentPackage
        updatedPackage =
          { currentPackage | tests = Dict.remove key currentPackage.tests }
      in
        ( { model
            | currentPackage = updatedPackage
            , packages = Dict.insert (Uuid.toString currentPackage.uuid) currentPackage model.packages
          }
        , persistPackage updatedPackage
        )

fdg_update : Model -> FDG.Msg -> GraphPackage
fdg_update model updateMessage =
  let
    currentPackage = model.currentPackage
    newModel = 
      FDG.update
        ( if model.leftPanelOpen then
            60 + model.leftPanelWidth
          else
            60
        , 1
        )
        updateMessage
        model.currentPackage.model
  in
    { currentPackage | model = newModel }


calculateRightTopDimensions : Model -> ( Float, Float, GraphPackage )
calculateRightTopDimensions model =
  let
    (viewportWidth, viewportHeight) = model.mainPanelDimensions
    iconBarWidth = 60
    leftPanelWidth = if model.leftPanelOpen then model.leftPanelWidth else 0
    statusBarHeight = 30
    bottomPanelHeight = if model.rightBottomPanelOpen then model.rightBottomPanelHeight else 0
    -- Always account for some splitter height (either full splitter or collapsed splitter)
    splitterHeight = if model.rightBottomPanelOpen then 8 else 4
    
    rightTopWidth = viewportWidth - iconBarWidth - leftPanelWidth
    rightTopHeight = viewportHeight - statusBarHeight - bottomPanelHeight - splitterHeight
    newGraph =
      fdg_update
        model
        (FDG.ViewportUpdated (rightTopWidth, rightTopHeight))
  in
  ( rightTopWidth, rightTopHeight, newGraph )

-- VIEW

view : Model -> Html Msg
view model =
  div 
    [ HA.class "main-container layout-flex-column" ]
    [ div 
      [ HA.class "main-content" ]
      [ viewLeftSection model
      , viewRightSection model
      ]
    , viewStatusBar model
    ]

viewLeftSection : Model -> Html Msg
viewLeftSection model =
  div 
    [ HA.class "left-section" ]
    [ viewIconBar model
    , if model.leftPanelOpen then
        div 
          [ HA.class "left-panel-container" ]
          [ viewLeftPanel model
          , viewHorizontalSplitter
          ]
      else
        text ""
    ]

viewIconBar : Model -> Html Msg
viewIconBar model =
  div 
    [ HA.class "icon-bar" ]
    [ viewIcon ComputationsIcon "📁" model
    , viewIcon TestsIcon "🧪" model
    , viewIcon SearchIcon "🔍" model
    , viewIcon GitIcon "🌿" model
    , viewIcon ExtensionsIcon "🧩" model
    ]

viewIcon : LeftPanelIcon -> String -> Model -> Html Msg
viewIcon icon iconText model =
  let
    isSelected = model.selectedIcon == Just icon
    iconClass = if isSelected then "icon icon--selected" else "icon"
  in
  div 
    [ HA.class iconClass
    , onClick (ClickIcon icon)
    ]
    [ text iconText ]














viewNode : FDG.Model -> Graph.Node FDG.Entity -> Svg msg
viewNode { userGraph, currentOperation, disconnectedNodes, execution } { label, id } =
  let
    graphNode =
      Graph.get id userGraph.graph
    thisNodeIsTerminal =
      Maybe.map Automata.Data.isTerminalNode graphNode
      |> Maybe.withDefault False
  in
    g
      [ class [ "state-node" ]
      , TypedSvg.Attributes.pointerEvents "none"
      ]
      [ circle
          [ r FDG.nodeRadius
          , strokeWidth 2
          , cx label.x
          , cy label.y
          , class
              ( if id == userGraph.root then [ "start" ] else [] )
          ]
          []
       ,  if thisNodeIsTerminal && id == userGraph.root then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ TypedSvg.Core.text "💥"
              ]
          else if thisNodeIsTerminal then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ TypedSvg.Core.text "🎯"
              ]
          else if id == userGraph.root then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 12
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              , fill <| Paint <| Color.grey
              ]
              [ TypedSvg.Core.text "⭐"
              ]
          else
            g [] []
      ]

viewLink : FDG.Model -> Edge Connection -> Svg Msg
viewLink ({ userGraph } as model) edge =
  let
    source =
      Maybe.withDefault (Force.entity 0 { }) <| Maybe.map (.node >> .label) <| Graph.get edge.from userGraph.graph

    target =
      Maybe.withDefault (Force.entity 0 { }) <| Maybe.map (.node >> .label) <| Graph.get edge.to userGraph.graph
    cardinality = FDG.identifyCardinality model edge
    positioning =
      FDG.path_between source target cardinality 7 7
    font_size = 16.0 -- this is the default, if not otherwise set
    labelText = FDG.connectionToSvgText edge.label
  in
    g
      [ TypedSvg.Attributes.pointerEvents "none"
      ]
      [
        path
          [ d positioning.pathString
          , noFill
          , class [ "link", "background" ]
          ]
          []
      , path
          [ strokeWidth 3
          , stroke <| Paint <| FDG.paletteColors.edge
          , d positioning.pathString
          , markerEnd "url(#arrowhead)"
          , noFill
          , class [ "link" ]
          ]
          []
      , text_
          [ x <| positioning.transition_coordinates.x
          , y <| positioning.transition_coordinates.y
          , fontFamily ["sans-serif"]
          , fontSize font_size
          , fontWeight FontWeightNormal
          , textAnchor AnchorMiddle
          , alignmentBaseline AlignmentCentral
          , Html.Attributes.attribute "paint-order" "stroke fill markers"
          , class [ "link" ]
          ]
          labelText
      ]

viewComputationThumbnail : Float -> GraphPackage -> Svg Msg
viewComputationThumbnail width ({ model } as package) =
  -- this takes the vast majority of its functionality from ForceDirectedGraph.elm
  svg
    [ viewBox 0 0 width (width / sqrt 2) -- 16:9 aspect ratio
    ]
    [ g
        [ transform [ Matrix 1 0 0 1 0 0 ] -- in case I need it later for some reason…
        ]
        [ defs [] [ FDG.arrowheadMarker, FDG.phantomArrowheadMarker ]
        , Graph.edges model.userGraph.graph
          |> List.filter (\edge -> not (Set.isEmpty edge.label))
          |> List.map (viewLink model)
          |> g [ class [ "links" ] ]
        , Graph.nodes model.userGraph.graph
          |> List.map (viewNode model)
          |> g [ class [ "nodes" ] ]
        ]
    ]

viewPackageItem : Model -> GraphPackage -> Html Msg
viewPackageItem model package =
  let
    uuidString = Uuid.toString package.uuid
    displayName = 
      package.description 
      |> Maybe.withDefault ("Computation " ++ String.left 8 uuidString)
    displaySvg =
      viewComputationThumbnail model.leftPanelWidth package
  in
  div 
    [ HA.class "left-panel__packageItem"
    , HA.css
        [ Css.position Css.relative
        , Css.borderRadius (Css.px 5)
        , Css.borderWidth (Css.px 1)
        , Css.borderColor (Css.rgb 0x28 0x2a 0x36) -- --dracula-background
        , Css.borderStyle Css.solid
        ]
    , onClick (SelectPackage package.uuid)
    ]
    [ Html.Styled.fromUnstyled <| displaySvg
    , div
        [ HA.css
            [ Css.position Css.absolute
            , Css.right (Css.px 5)
            , Css.top (Css.px 0)
            ]
        , HA.class "button"
        , HA.title "Delete computation"
        , onClick (DeletePackage package.uuid)
        ]
        [ text "🚮" ]
    ]

viewTestItem : (String, String) -> Html Msg
viewTestItem (key, test) =
  div 
    [ HA.class "left-panel__testItem"
    , HA.css
        [ Css.position Css.relative
        , Css.overflow Css.hidden
        , Css.textOverflow Css.ellipsis
        , Css.whiteSpace Css.nowrap
        , Css.cursor Css.pointer
        , Css.userSelect Css.none
          -- --dracula-background
        , Css.backgroundColor <| Css.rgb 0x28 0x2a 0x36
          -- --dracula-foreground
        , Css.color <| Css.rgb 0xf8 0xf8 0xf2
        , Css.padding (Css.px 6)
        , Css.borderRadius (Css.px 4)
        ]
    , onClick (SelectTest key)
    ]
    [ span
        [ HA.css
          [ Css.width (Css.pct 100)
          -- , Css.whiteSpace Css.nowrap
          , Css.overflow Css.hidden
          , Css.textOverflow Css.ellipsis
          , Css.whiteSpace Css.pre
          , Css.fontFamilyMany
              [ "Fira Code", "Inconsolata", "DejaVu Sans Mono"
              , "Liberation Mono", "Ubuntu Mono", "Cascadia Code"
              , "Cascadia Mono", "Consolas", "Lucida Console"
              , "Courier New" ]
              Css.monospace
          ]
        ]
        [ text test ]
    , div
        [ HA.css
            [ Css.position Css.absolute
            , Css.right (Css.px 5)
            , Css.top (Css.px 0)
            ]
        , HA.class "button"
        , HA.title "Delete test"
        , onClick (DeleteTest key)
        ]
        [ text "🚮" ]
    ]

viewLeftPanel : Model -> Html Msg
viewLeftPanel model =
  div 
    [ HA.class "left-panel"
    , HA.style "width" (String.fromFloat model.leftPanelWidth ++ "px")
    ]
    [ case model.selectedIcon of
        Just ComputationsIcon ->
          div []
            [ h3
                []
                [ text "Computations "
                , div
                    [ HA.class "button button--primary"
                    , onClick CreateNewPackage
                    , HA.title "Create new computation"
                    ]
                    [ text "➕" ]
                ]
            -- , p [] [ text "File management functionality would go here." ]
            , ul []
              ( model.packages
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy (Time.posixToMillis << .created)
                |> List.reverse
                |> List.map
                  (\pkg ->
                    li
                      [ HA.class "left-panel__packageItem" ]
                      [ viewPackageItem model pkg ]
                  )
              )
            ]
        
        Just SearchIcon ->
          div []
            [ h3 [] [ text "Search" ]
            , input 
              [ HA.type_ "text"
              , HA.placeholder "Search in files..."
              ] []
            , p [] [ text "Search results would appear here." ]
            ]
        
        Just GitIcon ->
          div []
            [ h3 [] [ text "Source Control" ]
            , p [] [ text "Git integration would go here." ]
            , div [ HA.class "panel-content" ]
              [ p [] [ text "• 3 changes" ]
              , p [] [ text "• 1 staged file" ]
              , p [] [ text "• main branch" ]
              ]
            ]
        
        Just TestsIcon ->
          div []
            [ h3
                []
                [ text "Tests "
                , div
                    [ HA.class "button button--primary"
                    , onClick CreateNewTest
                    , HA.title "Create new test"
                    ]
                    [ text "➕" ]
                ]
            -- , p [] [ text "File management functionality would go here." ]
            , ul []
              ( model.currentPackage.tests
                |> Dict.toList
                |> List.sortBy Tuple.second
                |> List.map
                  (\keyAndTest ->
                    li
                      [ HA.class "left-panel__testItem" ]
                      [ viewTestItem keyAndTest ]
                  )
              )
            ]
        
        Just ExtensionsIcon ->
          div []
            [ h3 [] [ text "Extensions" ]
            , p [] [ text "Extension management would go here." ]
            , div [ HA.class "panel-content" ]
              [ p [] [ text "🔧 Elm Language Support" ]
              , p [] [ text "🎨 Dracula Theme" ]
              , p [] [ text "📝 Auto Format" ]
              ]
            ]
        
        Nothing ->
          text ""
    ]

viewHorizontalSplitter : Html Msg
viewHorizontalSplitter =
  div 
    [ HA.class "horizontal-splitter"
    , onMouseDown StartDraggingHorizontalSplitter
    ]
    []

viewRightSection : Model -> Html Msg
viewRightSection model =
  div 
    [ HA.class "right-section" ]
    [ viewRightTopPanel model
    , if model.rightBottomPanelOpen then
        div []
          [ viewVerticalSplitter
          , viewRightBottomPanel model
          ]
      else
        -- Show a minimal splitter when panel is closed
        viewCollapsedVerticalSplitter
    ]

viewRightTopPanel : Model -> Html Msg
viewRightTopPanel model =
  let
    (width, height) = model.rightTopPanelDimensions
  in
  div 
    [ HA.class "right-top-panel" ]
    [ -- For now, display dimensions as requested in comments
      div 
        [ HA.class "right-top-panel__content" ]
        [ Html.Styled.fromUnstyled
            ( FDG.view model.currentPackage.model
              |> Html.map ForceDirectedMsg
            )
        ]
    ]

viewVerticalSplitter : Html Msg
viewVerticalSplitter =
  div 
    [ HA.class "vertical-splitter"
    , onMouseDown StartDraggingVerticalSplitter
    ]
    [ div 
      [ HA.class "vertical-splitter__handle"
      , onClick ToggleBottomPanel
      ]
      []
    ]

viewCollapsedVerticalSplitter : Html Msg
viewCollapsedVerticalSplitter =
  div 
    [ HA.class "collapsed-vertical-splitter"
    , onClick ToggleBottomPanel
    ]
    [ div 
      [ HA.class "collapsed-vertical-splitter__handle" ]
      []
    ]

viewRightBottomPanel : Model -> Html Msg
viewRightBottomPanel model =
  div 
    [ HA.class "right-bottom-panel"
    , HA.style "height" (String.fromFloat model.rightBottomPanelHeight ++ "px")
    ]
    [ viewBottomPanelHeader model
    , viewBottomPanelContent model
    ]

viewTestPanelButtons : Model -> List (Html Msg)
viewTestPanelButtons model =
  [ div
    [ HA.class (getActionButtonClass model.executionState RunExecution)
    , onClick RunExecution
    , HA.disabled (model.executionState == ExecutionComplete || not (FDG.canExecute model.currentPackage.model))
    , HA.title "Run"
    ]
    [ text "▶️" ]
  , div
    [ HA.class (getActionButtonClass model.executionState ResetExecution)
    , onClick ResetExecution
    , HA.disabled (model.executionState == Ready || model.executionState == NotReady)
    , HA.title "Reset"
    ]
    [ text "⏹️" ]
  , div
    [ HA.class (getActionButtonClass model.executionState StepThroughExecution)
    , onClick StepThroughExecution
    , HA.disabled (model.executionState == ExecutionComplete || not (FDG.canExecute model.currentPackage.model))
    , HA.title "Step-through"
    ]
    [ text "⏭️" ]
  , div
    [ HA.class (getActionButtonClass model.executionState (DeleteTest model.currentPackage.currentTestKey))
    , HA.css
        [ Css.marginTop (Css.px 30)
        ]
    , onClick <| DeleteTest model.currentPackage.currentTestKey
    , HA.disabled (model.executionState == StepThrough)
    , HA.title "Delete test"
    ]
    [ text "🚮" ]
  ]

viewDescriptionPanelButtons : Model -> List (Html Msg)
viewDescriptionPanelButtons model =
  []

viewBottomPanelHeader : Model -> Html Msg
viewBottomPanelHeader model =
  let
    buttons =
      ( case model.selectedBottomPanel of
          AddTestPanel ->
            viewTestPanelButtons model
          EditDescriptionPanel ->
            viewDescriptionPanelButtons model
      )
  in
  div 
    [ HA.classList
        [ ("bottom-panel__header", True)
        , ("open", not <| List.isEmpty buttons)
        ]
    ]
    [ div 
      [ HA.class "bottom-panel__actions" ]
      buttons
    ]

executionText : Model -> Html a
executionText { currentPackage } =
  div
    []
    [ case currentPackage.model.execution of
        Nothing ->
          p [] [ text "🦗 Bug! K%UGFCR" ] -- eheh?! I should never be here!
        Just result ->
          let
            maybeDatum = FDG.executionData result
          in
            p
              [ HA.class "output-line" ]
              [ text <|
                  case result of
                    EndOfInput (Accepted _) ->
                        "✅ Success! 🎉"
                    EndOfInput (Rejected _) ->
                        "❌ Rejected 😔.  The computation did not end with an accepting transition."
                    EndOfComputation _ ->
                        "❌ Rejected 😔.  The input was longer than the computation."
                    CanContinue _ ->
                        "🟢 Continuing execution."
                    x ->
                        "🦗 Bug!  " ++ Debug.toString x ++ ".  You should never see this message.  I need to figure out what just happened here…"
              , Maybe.map
                  (\datum ->
                    p
                      [ HA.class "computation-progress" ]
                      [ span
                          [ HA.class "computation-progress-processed" ]
                          ( datum.transitionsTaken
                            |> List.reverse
                            |> List.map 
                              (\(_, (ch, isFinal)) ->
                                span
                                  [ HA.class <| if isFinal == 1 then "final" else "non-final" ]
                                  [ text <| FDG.textChar ch ]
                              )
                          )
                      , span
                          [ HA.class "computation-progress-to-do" ]
                          [ List.map (\ch -> FDG.textChar ch) datum.remainingData
                            |> String.concat
                            |> text
                          ]
                      ]
                  )
                  maybeDatum
                |> Maybe.withDefault (text "")
              ]
    ]

viewAddTestPanelContent : Model -> Html Msg
viewAddTestPanelContent model =
  let
    testContent =
      Dict.get model.currentPackage.currentTestKey model.currentPackage.tests
      |> Maybe.withDefault ""
  in
    case model.executionState of
      Ready ->
        textarea 
          [ HA.class "right-bottom-panel__textarea"
          , HA.value testContent
          , onInput UpdateTestPanelContent
          , HA.placeholder "Enter your test input here"
          , HA.disabled <| Maybe.Extra.isJust model.currentPackage.model.currentOperation
          ]
          []

      NotReady ->
        div
          []
          [ div 
              [ HA.class "notready-output" ]
              [ p [ HA.class "output-line" ] [ text "All changes must be committed or undone before you can execute." ] ]
          , textarea
              [ HA.class "right-bottom-panel__textarea"
              , HA.value testContent
              , onInput UpdateTestPanelContent
              , HA.placeholder "Enter your test input here"
              , HA.disabled <| Maybe.Extra.isJust model.currentPackage.model.currentOperation
              ]
              []
          ]
      
      ExecutionComplete ->
        div 
          [ HA.class "execution-output" ]
          [ executionText model ]
      StepThrough ->
        div 
          [ HA.class "debug-output" ]
          [ executionText model ]

viewEditDescriptionPanelContent : Model -> Html Msg
viewEditDescriptionPanelContent model =
  textarea 
    [ HA.class "right-bottom-panel__textarea"
    , HA.value (model.currentPackage.description |> Maybe.withDefault "")
    , onInput UpdateDescriptionPanelContent
    , HA.placeholder "What does this computation do?"
    , HA.disabled <| Maybe.Extra.isJust model.currentPackage.model.currentOperation
    ]
    []

viewBottomPanelContent : Model -> Html Msg
viewBottomPanelContent model =
  div 
    [ HA.class "bottom-panel__content" ]
    [  div 
      [ HA.class "bottom-panel__titlebar" ]
      [ div
        [ HA.class "bottom-panel__title" ]
        [ text (getBottomPanelTitle model.selectedBottomPanel model.executionState) ]
      , div
        [ HA.class "bottom-panel__tab-buttons" ]
        [ div
          [ HA.classList
              [ ("button", True)
              , ("tab-button", True)
              , ("tab-button--selected", model.selectedBottomPanel == AddTestPanel)
              ]
          , onClick <| SelectBottomPanel AddTestPanel
          , HA.title "Add test"
          ]
          [ text "🧪" ]
        , div
          [ HA.classList
              [ ("button", True)
              , ("tab-button", True)
              , ("tab-button--selected", model.selectedBottomPanel == EditDescriptionPanel)
              ]
          , onClick <| SelectBottomPanel EditDescriptionPanel
          , HA.title "Describe computation"
          ]
          [ text "🗃️" ]
        ]
      ]
    , case model.selectedBottomPanel of
        AddTestPanel ->
          viewAddTestPanelContent model
        EditDescriptionPanel ->
          viewEditDescriptionPanelContent model
    ]

getBottomPanelTitle : BottomPanel -> ExecutionState -> String
getBottomPanelTitle panel state =
  case panel of
    AddTestPanel ->
      case state of
        Ready -> "Testing // Ready"
        ExecutionComplete -> "Testing // Execution complete" -- show execution output
        StepThrough -> "Testing // Step-through"
        NotReady -> "Testing // Not ready"
    EditDescriptionPanel ->
      "Describe computation"

getActionButtonClass : ExecutionState -> Msg -> String
getActionButtonClass currentState buttonAction =
  let
    baseClass = "button action-button"
    activeClass = case (currentState, buttonAction) of
      (ExecutionComplete, RunExecution) -> " action-button--active"
      (StepThrough, StepThroughExecution) -> " action-button--active"
      _ -> ""
  in
  baseClass ++ activeClass

viewStatusBar : Model -> Html Msg
viewStatusBar model =
  div 
    [ HA.class "status-bar" ]
    [ span [] [ text (getStatusMessage model.executionState) ]
    , div 
      [ HA.class "status-bar__section" ]
      [ div
        [ HA.classList
            [ ("button", True)
            , ("status-bar__button", True)
            , ("status-bar__button--active", model.rightBottomPanelOpen)
            ]
        , onClick ToggleBottomPanel
        ]
        [ text "Terminal" ]
      ]
    , span 
      [ HA.class "status-bar__section--right" ]
      [ text ("Viewport: " ++ String.fromFloat (Tuple.first model.mainPanelDimensions) ++ " × " ++ String.fromFloat (Tuple.second model.mainPanelDimensions)) ]
    ]

getStatusMessage : ExecutionState -> String
getStatusMessage state =
  case state of
    Ready -> "Ready"
    ExecutionComplete -> "Running..."
    StepThrough -> "Debug Mode"
    NotReady -> "Uncommitted"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ FDG.subscriptions
        model.rightTopPanelDimensions
        model.currentPackage.model
      |> Sub.map ForceDirectedMsg
    , BE.onResize (\w h -> OnResize (toFloat w, toFloat h))
    , Time.every 25000 -- 'sup, homie Nyquist? All good?
        ( Time.posixToMillis
          >> (\v -> (v // (1000 * 60)) * (1000 * 60))
          >> Time.millisToPosix
          >> Seconded
        )
    , if model.isDraggingHorizontalSplitter || model.isDraggingVerticalSplitter then
        Sub.batch
          [ BE.onMouseMove (D.map2 MouseMove (D.field "clientX" D.float) (D.field "clientY" D.float))
          , BE.onMouseUp (D.succeed StopDragging)
          ]
      else
        Sub.none
    ]