module Main exposing (..)
import Browser
import Browser.Events as BE
import Html.Styled exposing
  (Html, div, h3, p, ul, li, input, textarea, span, toUnstyled, text)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown)
import Json.Encode as E
import Json.Decode as D
import ForceDirectedGraph as FDG
import Automata.Data exposing (..)
import Platform.Cmd as Cmd
import Html.Styled.Attributes as HA
import Html
import Maybe.Extra
import Css
import Platform.Cmd as Cmd
import Uuid
import Random.Pcg.Extended as Random
import Time
import Dict
import Ports
import Platform.Cmd as Cmd
import Automata.DFA as DFA
import TypedSvg exposing (g)
import TypedSvg.Attributes.InPx exposing (x, y, height, width)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import Html.Styled exposing (h2, h4)
import AutoSet
import TypedSvg.Core exposing (Svg)
import Dict exposing (Dict)
import Svg
import AutoDict
import Uuid exposing (Uuid)

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

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
  | UpdateTestPanelContent String TestExpectation
  | UpdateDescriptionPanelContent String
  | UpdateRightTopDimensions Float Float
  | RunExecution
  | ResetExecution
  | StepThroughExecution
  | SelectBottomPanel BottomPanel
  | CreateNewPackage
  | SelectPackage Uuid
  | DeletePackage Uuid
  | SelectTest Uuid
  | DeleteTest Uuid
  | CreateNewTest

type alias Model = Main_Model

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

encodeTest : Test -> E.Value
encodeTest { input, expectation } =
  E.object
    [ ("input", E.string input)
    , ("expectation", E.bool (expectation == ExpectAccepted))
    -- do not store the result. It must be recalculated each time.
    ]

decodeTest : AutomatonGraph -> D.Decoder Test
decodeTest g =
  D.map2
    (\input expectation ->
      { input = input
      , expectation = expectation
      , result =
          DFA.stepThroughInitial input g
          |> DFA.run g
      })
    (D.field "input" D.string)
    (D.field "expectation" <| D.map (\b -> if b then ExpectAccepted else ExpectRejected) D.bool)

encodeGraphPackage : GraphPackage -> E.Value
encodeGraphPackage pkg =
  E.object
    [ ("graph", DFA.encodeAutomatonGraph pkg.userGraph)
    , ("dimensions",
        E.object
          [ ("w", E.float <| Tuple.first pkg.dimensions)
          , ("h", E.float <| Tuple.second pkg.dimensions)
          ]
      )
    , ("description", Maybe.map E.string pkg.description |> Maybe.withDefault E.null)
    , ("created", E.int (Time.posixToMillis pkg.created))
    , ("tests", DFA.encodeAutoDict Uuid.toString encodeTest pkg.tests)
    , ("currentTestKey", Uuid.encode pkg.currentTestKey)
    ]

decodeGraphPackage : D.Decoder GraphPackage
decodeGraphPackage =
  D.field "graph" DFA.decodeAutomatonGraph
  |> D.andThen
    (\graph ->
      D.map5 (GraphPackage graph)
        ( D.map2
            (\w h -> (w, h))
            (D.at ["dimensions", "w"] D.float)
            (D.at ["dimensions", "h"] D.float)
        )
        (D.field "description" <| D.oneOf [ D.null Nothing, D.map Just D.string ])
        (D.field "created" <| D.map Time.millisToPosix D.int)
        (D.field "currentTestKey" Uuid.decoder)
        (D.field "tests" <| DFA.decodeAutoDict Uuid.toString Uuid.fromString (decodeTest graph))
    )

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
        (D.field "packages" <| D.list decodeGraphPackage)
    )

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    decoded =
      D.decodeValue decodeFlags flags
      |> Result.mapError (\err -> Debug.log "OH NO! FLAGS WAS NOT PARSED CORRECTLY!!" err)
      |> Result.withDefault (Flags 80 60 0 [] (Time.millisToPosix 0) [])
    
    initialRightTopWidth = decoded.width - 60 - 4 - 1 - 2  -- 60px for icon bar, 2px border each side, 1px icon-bar border + 1px each side top-right border
    initialRightTopHeight = decoded.height - 218  -- 185px bottom panel + 1px border each side + 8px splitter + 30px status bar + 1px status-bar border
    initialSeed = Random.initialSeed decoded.initialSeed decoded.extendedSeeds
    packages =
      decoded.packages
      -- |> List.map (\v -> Automata.Debugging.debugAutomatonGraph "Loaded" v.model.userGraph |> \_ -> v)
      |> List.map (\v -> ( v.userGraph.graphIdentifier, v ))
      |> AutoDict.fromList Uuid.toString
  in
    ( { fdg_model =
          FDG.init
            (initialRightTopWidth, initialRightTopHeight)
            decoded.startTime
            initialSeed
            packages
      , mainPanelDimensions =
          ( decoded.width - 4 -- 2px border all around main-container
          , decoded.height - 4 -- same border as above
          )
      , leftPanelOpen = False
      , selectedIcon = Nothing
      , leftPanelWidth = 250
      , rightBottomPanelOpen = True
      , rightBottomPanelHeight = 185
      , rightTopPanelDimensions = ( initialRightTopWidth, initialRightTopHeight )
      , isDraggingHorizontalSplitter = False
      , isDraggingVerticalSplitter = False
      , executionStage = Ready
      , selectedBottomPanel = AddTestPanel
      }
    , Cmd.none
    )

-- UPDATE

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
        newFdModel =
          FDG.update fdMsg model.fdg_model
          -- |> \v ->
          --   if fdMsg /= FDG.Tick then
          --     Debug.log "msg" fdMsg |> \_ ->
          --     FDG.debugModel_ "updated" v
          --   else
          --     v
        preUpdateChangeCount =
          List.length model.fdg_model.undoBuffer
        isCommitlike =
          preUpdateChangeCount > 0 && newFdModel.undoBuffer == []
      in
      ( { model
          | executionStage =
              if FDG.canExecute newFdModel then
                if model.executionStage == NotReady then
                  Ready
                else
                  model.executionStage
              else
                NotReady
          , fdg_model = newFdModel
        }
      , if isCommitlike then
          persistPackage model.fdg_model.currentPackage
        else
          Cmd.none
      )

    OnResize (width, height) ->
      let
        newModel =
          { model
            -- When I receive this, it is the size of the Application in the
            -- x-direction EXCLUDING external borders, but it is the size of
            -- the Application in the y-direction INCLUDING external borders.
            -- No; I don't understand that at all.  But it's what I have to
            -- work with, I guess.
            | mainPanelDimensions = (width - 4, height)
          }
        (newRightTopWidth, newRightTopHeight, newFdgModel) = calculateRightTopDimensions newModel
      in
      ( { newModel
          | rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
          , fdg_model = newFdgModel
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
        (newRightTopWidth, newRightTopHeight, newFdgModel) =
          calculateRightTopDimensions { model | leftPanelOpen = newLeftPanelOpen }
      in
      ( { model 
        | leftPanelOpen = newLeftPanelOpen
        , selectedIcon = newSelectedIcon
        , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
        , fdg_model = newFdgModel
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
      if model.isDraggingHorizontalSplitter then
        let
          (viewportWidth, _) = model.mainPanelDimensions
          leftOffset =
            60 -- icon-bar width
            + 1 -- icon-bar right-border
            + 20 -- left-panel left-padding
            + 20 -- left-panel right-padding
            + 1 -- left-panel right-border
            + 4 -- half the splitter-width
            + 2 -- …it looks visually nicer, because the cursor takes up some area
          minWidth = 100 + leftOffset
          maxWidth = viewportWidth / 2
          newLeftPanelWidth = clamp minWidth maxWidth (x - leftOffset)
        in
          ( recalculateUI { model | leftPanelWidth = newLeftPanelWidth }
          , Cmd.none
          )
      else if model.isDraggingVerticalSplitter then
        let
          (_, viewportHeight) = model.mainPanelDimensions
          minHeight = 185  -- 8em @ 16px font ≈ 128px
          maxHeight = viewportHeight / 2
          statusBarHeight = 30
          newBottomHeight = clamp minHeight maxHeight (viewportHeight - y - statusBarHeight)
        in
        ( recalculateUI { model | rightBottomPanelHeight = newBottomHeight }
        , Cmd.none
        )
      else
        ( model, Cmd.none )

    ToggleBottomPanel ->
      ( recalculateUI { model | rightBottomPanelOpen = not model.rightBottomPanelOpen }
      , Cmd.none
      )

    UpdateTestPanelContent newInput expectation ->
      let
        pkg = model.fdg_model.currentPackage
        updatedTests =
          case newInput of
            "" ->
              AutoDict.remove pkg.currentTestKey pkg.tests
            _ ->
              AutoDict.insert
                pkg.currentTestKey
                  ( Test
                      newInput
                      expectation
                      ( DFA.stepThroughInitial newInput pkg.userGraph
                        |> DFA.run pkg.userGraph
                      )
                  )
                pkg.tests
        updatedPackage =
          { pkg | tests = updatedTests }
      in
        ( { model
            | fdg_model =
                FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
          }
        , persistPackage updatedPackage
        )

    UpdateDescriptionPanelContent content ->
      let
        currentPackage = model.fdg_model.currentPackage
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
            | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
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
        | executionStage = Ready
        , fdg_model =
            FDG.update
              ( FDG.Load
                  ( AutoDict.get model.fdg_model.currentPackage.currentTestKey model.fdg_model.currentPackage.tests
                    |> Maybe.map .input
                    |> Maybe.withDefault ""
                  )
              )
              model.fdg_model
            |> FDG.update FDG.Run
        }
      , Cmd.none
      )

    ResetExecution ->
      ( { model 
        | executionStage = Ready
        , fdg_model = FDG.update FDG.Stop model.fdg_model
        }
      , Cmd.none
      )

    StepThroughExecution ->
      let
        newFdModel =
          if model.executionStage /= StepThrough then
            -- this is the first click of stepping, so do a load first.
            FDG.update
              ( FDG.Load
                  ( AutoDict.get model.fdg_model.currentPackage.currentTestKey model.fdg_model.currentPackage.tests
                    |> Maybe.map .input
                    |> Maybe.withDefault ""
                  )
              )
              model.fdg_model
          else
            FDG.update FDG.Step model.fdg_model
      in
      ( { model 
        | executionStage =
            case newFdModel.execution of
              Nothing ->
                model.executionStage
              Just (CanContinue _) ->
                StepThrough
              _ ->
                ExecutionComplete
        , fdg_model =
            newFdModel
        }
      , Cmd.none
      )

    SelectBottomPanel p ->
      ( { model | selectedBottomPanel = p }
      , Cmd.none
      )

    CreateNewPackage ->
        ( { model | fdg_model = FDG.reInitialize model.fdg_model }
        , Cmd.none
        )

    SelectPackage uuid ->
      case AutoDict.get uuid model.fdg_model.packages of
        Nothing ->
          ( model, Cmd.none )
        Just pkg ->
          let
            fdg_model = model.fdg_model
            newFdgModel =
              { fdg_model | currentPackage = { pkg | dimensions = fdg_model.dimensions } }
              |> FDG.update (FDG.ViewportUpdated model.fdg_model.dimensions)
              |> FDG.update FDG.Reheat
          in
          ( { model
              | fdg_model = newFdgModel
            }
          , Cmd.none
          )

    DeletePackage uuid ->
      let
        fdg_model = model.fdg_model
        newPackages = AutoDict.remove uuid model.fdg_model.packages
        newFdgModel = { fdg_model | packages = newPackages }
      in
        ( if uuid == fdg_model.currentPackage.userGraph.graphIdentifier then
            { model | fdg_model = FDG.reInitialize newFdgModel }
          else
            { model | fdg_model = newFdgModel }
        , Ports.deleteFromStorage (Uuid.toString uuid)
        )

    CreateNewTest ->
      let
        fdg_model = model.fdg_model
        currentPackage = fdg_model.currentPackage
        (uuid, newSeed) = Random.step Uuid.generator fdg_model.uuidSeed
        newFdgModel =
          { fdg_model
            | currentPackage = { currentPackage | currentTestKey = uuid }
            , uuidSeed = newSeed
          }
      in
        ( { model | fdg_model = newFdgModel }
        , Cmd.none
        )

    SelectTest key ->
      let
        currentPackage = model.fdg_model.currentPackage
        updatedPackage = { currentPackage | currentTestKey = key }
      in
        ( { model
            | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
          }
        , Cmd.none -- no need to persist this though
        )

    DeleteTest key ->
      let
        currentPackage = model.fdg_model.currentPackage
        updatedPackage =
          { currentPackage | tests = AutoDict.remove key currentPackage.tests }
      in
        ( { model
            | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
          }
        , persistPackage updatedPackage
        )

calculateRightTopDimensions : Model -> ( Float, Float, FDG.Model )
calculateRightTopDimensions model =
  let
    -- the main panel dimensions are the size of the application area.
    (viewportWidth, viewportHeight) = model.mainPanelDimensions
    -- the icon bar is 60px wide. Not included: a 1px right-border.
    iconBarWidth = 60
    -- the left-panel also includes its right-border size here, for convenience.
    -- and it also includes 20px of padding on either side, if it's open.
    leftPanelWidth = if model.leftPanelOpen then model.leftPanelWidth + 1 + 20 + 20 else 0
    -- the status bar is 30px tall.  Not included: a 1px top border.
    statusBarHeight = 30
    -- the bottom-right panel is a particular height. Included: a 1px border on all sides.
    bottomPanelHeight = if model.rightBottomPanelOpen then model.rightBottomPanelHeight + 2 else 0
    -- Always account for some splitter height (either full splitter or collapsed splitter)
    leftRightSplitterWidth = if model.leftPanelOpen then 8 else 0
    rightTopBottomSplitterHeight = if model.rightBottomPanelOpen then 8 else 4
    -- add in: the 1px border from iconBar
    -- Also, the right-panel has a 1px border all around; add it in.
    rightTopWidth = viewportWidth - iconBarWidth - leftPanelWidth - leftRightSplitterWidth - 1 - 2
    -- add in: the 1px statusBar  border
    -- Also, the right-panel has a 1px border all around; add it in.
    rightTopHeight = viewportHeight - statusBarHeight - bottomPanelHeight - rightTopBottomSplitterHeight - 1 - 2
    newFdgModel =
      FDG.update
        (FDG.ViewportUpdated (rightTopWidth, rightTopHeight))
        model.fdg_model        
  in
  -- Debug.log
  --   ( "Dimension calculation"
  --   ++"\nApplication area width & height: " ++ Debug.toString (viewportWidth, viewportHeight)
  --   ++"\nWidth calculation:"
  --   ++"\n   Icon-bar width (excl. 1px right-border): " ++ String.fromFloat iconBarWidth
  --   ++"\n   Left-panel width (INCL. 1px right-border + 20px padding on each side): " ++ String.fromFloat leftPanelWidth
  --   ++"\n   Left-right splitter width: " ++ String.fromFloat leftRightSplitterWidth
  --   ++"\n   Calculation: " ++ String.fromFloat viewportWidth ++ " [total width] "
  --   ++" - " ++ String.fromFloat iconBarWidth ++ " [icon-bar width] "
  --   ++" - " ++ String.fromFloat leftPanelWidth ++ " [left-panel width] "
  --   ++" - " ++ String.fromFloat leftRightSplitterWidth ++ " [left-right splitter width] "
  --   ++" - 1 [icon-bar border] - 2 [right-panel 1px border] = " ++ String.fromFloat rightTopWidth
  --   ++"\nHeight calculation:"
  --   ++"\n   Status-bar height (excl. 1px top-border): " ++ String.fromFloat statusBarHeight
  --   ++"\n   Bottom-panel height (INCL. 1px border on each side): " ++ String.fromFloat bottomPanelHeight
  --   ++"\n   Right-top-bottom splitter height: " ++ String.fromFloat rightTopBottomSplitterHeight
  --   ++"\n   Calculation: " ++ String.fromFloat viewportHeight ++ " [total height] "
  --   ++" - " ++ String.fromFloat statusBarHeight ++ " [status-bar height] "
  --   ++" - " ++ String.fromFloat bottomPanelHeight ++ " [bottom-panel height] "
  --   ++" - " ++ String.fromFloat rightTopBottomSplitterHeight ++ " [right-top-bottom splitter height] "
  --   ++" - 1 [status-bar border] - 1 [bottom-panel border] - 2 [right-panel 1px border] = " ++ String.fromFloat rightTopHeight
  --   ) () |> \_ ->
  ( rightTopWidth, rightTopHeight, newFdgModel )

recalculateUI : Model -> Model
recalculateUI model =
  let
    ( rightTopWidth, rightTopHeight, newFdgModel ) =
      calculateRightTopDimensions model
  in
    { model
      | fdg_model = newFdgModel
      , rightTopPanelDimensions = ( rightTopWidth, rightTopHeight )
    }

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

isAccepted : ExecutionResult -> Maybe Bool
isAccepted result =
  case result of
    InternalError -> Nothing
    CanContinue _ -> Nothing
    EndOfInput (Accepted _) -> Just True
    _ -> Just False

isPassingTest : Test -> Maybe Bool
isPassingTest test =
  isAccepted test.result
  |> Maybe.map
    (\v ->
      (test.expectation == ExpectAccepted && v) || (test.expectation == Automata.Data.ExpectRejected && not v)
    )

isFailingTest : Test -> Maybe Bool
isFailingTest = isPassingTest >> Maybe.map not

viewIconBar : Model -> Html Msg
viewIconBar model =
  let
    (pass, fail, error) =
      AutoDict.toList model.fdg_model.currentPackage.tests
      |> List.foldl
        (\(_, {expectation, result}) (p, f, e) ->
          case isAccepted result of
            Just True ->
              if expectation == ExpectAccepted then
                (p + 1, f, e)
              else
                (p, f + 1, e)
            Just False ->
              if expectation == ExpectRejected then
                (p + 1, f, e)
              else
                (p, f + 1, e)
            Nothing -> (p, f, e + 1)
        )
        (0, 0, 0)
    (testBackgroundColor, number, testTitle) =
      case (pass, fail, error) of
        (0, 0, 0) ->
          ( Css.rgb 0xf8 0xf8 0xf2 -- --dracula-foreground
          , "…"
          , "No tests exist"
          )
        (_, 0, 0) -> -- no failures, no errors, and only passes.
          ( Css.rgb 0x8c 0xf1 0x8c -- --dracula-green
          , "💯"
          , "All tests passed!"
          )
        (0, _, 0) -> -- no passes, no errors, only failures.
          ( Css.rgb 0xff 0x55 0x55 -- --dracula-red
          , "😵‍💫"
          , "All tests failed!"
          )
        (_, _, 0) -> -- some passes, some failures, no errors
          ( Css.rgb 0xf1 0x8c 0x8c -- --dracula-pink
          , String.fromInt fail
          , if fail == 1 then
              "1 test failed"
            else
              String.fromInt fail ++ " tests failed"
          )
        _ -> -- internal error exists!
          ( Css.rgb 0xf1 0xfa 0x8c -- --dracula-yellow
          , "❓"
          , "Some tests did not complete due to an internal error! This indicates a problem with the computation system. Please report it to the developer."
          )
    testsExtra =
      [ div
          [ HA.class "test-panel__number"
          , HA.css
              [ Css.backgroundColor <| testBackgroundColor
              , Css.color <| Css.rgb 0x28 0x2a 0x36 -- --dracula-background
              , Css.borderRadius (Css.pct 50)
              -- , Css.borderColor <| Css.rgb 0x44 0x47 0x5a -- --dracula-current-line
              -- , Css.borderWidth (Css.px 1)
              -- , Css.borderStyle Css.solid
              , Css.position Css.absolute
              , Css.left (Css.pct 50)
              , Css.top (Css.pct 50)
              -- , Css.fontSize (Css.pct 60)
              , Css.padding (Css.px 2)
              -- , Css.opacity (Css.num 0.8)
              , Css.userSelect Css.none
              , Css.width (Css.em 1.2)
              , Css.height (Css.em 1.2)
              , Css.lineHeight (Css.em 1.2)
              , Css.textAlign Css.center
              , Css.fontWeight Css.bold
              -- , let
              --     o = 0.5
              --     blur = 1
              --   in
              --     Css.textShadowMany
              --     [ { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px o, offsetY = Css.px o, color = Just <| testBackgroundColor }
              --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px -o, offsetY = Css.px -o, color = Just <| testBackgroundColor }
              --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px -o, offsetY = Css.px o, color = Just <| testBackgroundColor }
              --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px o, offsetY = Css.px -o, color = Just <| testBackgroundColor }
              --     ]
              ]
          , HA.title testTitle
          ]
          [ text number ]
      ]
  in
    div 
      [ HA.class "icon-bar" ]
      [ viewIcon ComputationsIcon "📁" [] model
      , viewIcon TestsIcon "🧪" testsExtra model
      , viewIcon SearchIcon "🔍" [] model
      , viewIcon GitIcon "🌿" [] model
      , viewIcon ExtensionsIcon "🧩" [] model
      ]

viewIcon : LeftPanelIcon -> String -> List (Html Msg) -> Model -> Html Msg
viewIcon icon iconText extra model =
  let
    isSelected = model.selectedIcon == Just icon
    iconClass = if isSelected then "icon icon--selected" else "icon"
  in
  div 
    [ HA.class iconClass
    , onClick (ClickIcon icon)
    , HA.css
        [ Css.position Css.relative
        ]
    ]
    ( text iconText :: extra )






    

viewPackageItem : Model -> GraphPackage -> Html Msg
viewPackageItem model package =
  let
    displaySvg =
      FDG.viewComputationThumbnail (model.leftPanelWidth - 15) model.fdg_model package
    canSelect =
      -- we can select this EITHER if there are no pending changes, OR
      -- if this is the currently-loaded package (i.e. to "reset"/"refresh" it)
      model.fdg_model.undoBuffer == [] ||
      package.userGraph.graphIdentifier == model.fdg_model.currentPackage.userGraph.graphIdentifier
  in
  div 
    [ HA.classList
        [ ("left-panel__packageItem", True)
        , ("left-panel__packageItem--disabled", not canSelect)
        ]
    , HA.css
        [ Css.position Css.relative
        , Css.borderRadius (Css.px 5)
        , Css.borderWidth (Css.px 1)
        , Css.borderColor (Css.rgb 0x28 0x2a 0x36) -- --dracula-background
        , Css.borderStyle Css.solid
        , Css.userSelect Css.none
        , if canSelect then
            Css.cursor Css.pointer
          else
            Css.cursor Css.notAllowed
        ]
    , if canSelect then
        onClick (SelectPackage package.userGraph.graphIdentifier)
      else
        HA.title "Apply or cancel the pending changes before selecting another package."
    ]
    [ Html.Styled.fromUnstyled <| Html.map ForceDirectedMsg displaySvg
    , div
        [ HA.css
            [ Css.position Css.absolute
            , Css.right (Css.px 5)
            , Css.top (Css.px 0)
            ]
        , HA.classList
            [ ("button", canSelect)
            , ("button--disabled", not canSelect)
            ]
        , HA.title "Delete computation"
        , if canSelect then
            onClick (DeletePackage package.userGraph.graphIdentifier)
          else
            HA.title "Apply or cancel the pending changes before deleting a package."
        ]
        [ text "🚮" ]
    ]

viewTestItemInPanel : (Uuid, Test) -> Html Msg
viewTestItemInPanel (key, test) =
  let
    testStatus = isFailingTest test
  in
    div 
      [ HA.classList
          [ ("left-panel__testItem", True)
          , ("left-panel__testItem--failing", testStatus == Just True)
          , ("left-panel__testItem--passing", testStatus == Just False)
          , ("left-panel__testItem--error", testStatus == Nothing)
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
          [ text test.input ]
      , div
          [ HA.css
              [ Css.position Css.absolute
              , Css.right (Css.px 5)
              , Css.top (Css.px 5)
              ]
          , HA.class "button"
          , HA.title "Delete test"
          , onClick (DeleteTest key)
          ]
          [ text "🚮" ]
      ]

viewLeftTestPanel : Model -> Html Msg
viewLeftTestPanel model =
  let
    (expectAccept, expectReject) =
      model.fdg_model.currentPackage.tests
      |> AutoDict.toList
      |> List.partition (Tuple.second >> .expectation >> (==) ExpectAccepted)
    displayTests headingHtml tx =
      case tx of
        [] ->
          text ""
        _ ->
          div
            []
            [ h4 [] headingHtml
            , ul []
              ( List.sortBy (Tuple.second >> .input) tx
                |> List.map
                  (\(key, test) ->
                    li
                      []
                      [ viewTestItemInPanel (key, test) ]
                  )
              )
            ]
  in
    div
      [ HA.css
          [ Css.backgroundColor <| Css.rgb 0x28 0x2a 0x36 -- --dracula-background
          , Css.color <| Css.rgb 0xf8 0xf8 0xf2 -- --dracula-foreground
          ]
      ]
      [ h2
          []
          [ text "Tests "
          , div
              [ HA.class "button button--primary"
              , onClick CreateNewTest
              , HA.title "Create new test"
              ]
              [ text "➕" ]
          ]
      , model.fdg_model.currentPackage.description
        |> Maybe.map
          (\desc ->
            div
              [ HA.css
                  [ Css.fontStyle Css.italic
                  , Css.fontSize (Css.rem 0.8)
                  , Css.whiteSpace Css.preWrap
                  ]
              ]
              [ text "“"
              , text desc
              , text "”"
              ]
          )
        |> Maybe.withDefault (text "")
      , displayTests
          [ span [] [ text "Should be " ]
          , span [ HA.css [ Css.color <| Css.rgb 0x50 0xfa 0x7b ] ] [ text "accepted" ]
          ]
          expectAccept
      , displayTests
          [ span [] [ text "Should be " ]
          , span [ HA.css [ Css.color <| Css.rgb 0xff 0x79 0xc6 ] ] [ text "rejected" ]
          ]
        expectReject
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
            [ h2
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
              ( model.fdg_model.packages
                |> AutoDict.toList
                |> List.map Tuple.second
                |> List.sortBy (Time.posixToMillis << .created)
                |> List.reverse
                |> List.map
                  (\pkg ->
                    li
                      []
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
          viewLeftTestPanel model
        
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
  div 
    [ HA.class "right-top-panel" ]
    [ -- For now, display dimensions as requested in comments
      div 
        [ HA.class "right-top-panel__content" ]
        [ Html.Styled.fromUnstyled
            ( FDG.view model.fdg_model
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
    [ HA.class (getActionButtonClass model.executionStage RunExecution)
    , onClick RunExecution
    , HA.disabled (model.executionStage == ExecutionComplete || not (FDG.canExecute model.fdg_model))
    , HA.title "Run"
    ]
    [ text "▶️" ]
  , div
    [ HA.class (getActionButtonClass model.executionStage ResetExecution)
    , onClick ResetExecution
    , HA.disabled (model.executionStage == Ready || model.executionStage == NotReady)
    , HA.title "Reset"
    ]
    [ text "⏹️" ]
  , div
    [ HA.class (getActionButtonClass model.executionStage StepThroughExecution)
    , onClick StepThroughExecution
    , HA.disabled (model.executionStage == ExecutionComplete || not (FDG.canExecute model.fdg_model))
    , HA.title "Step-through"
    ]
    [ text "⏭️" ]
  , div
    [ HA.class (getActionButtonClass model.executionStage (DeleteTest model.fdg_model.currentPackage.currentTestKey))
    , HA.css
        [ Css.marginTop (Css.px 15)
        ]
    , onClick <| DeleteTest model.fdg_model.currentPackage.currentTestKey
    , HA.disabled (model.executionStage == StepThrough)
    , HA.title "Delete test"
    ]
    [ text "🚮" ]
  ]

viewDescriptionPanelButtons : Model -> List (Html Msg)
viewDescriptionPanelButtons _ =
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
executionText { fdg_model } =
  div
    []
    [ case fdg_model.execution of
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
                          ( datum.transitions
                            |> List.reverse
                            |> List.map
                              (\{matching, consumed} ->
                                viewInputProcessing consumed
                                  ( if (AutoSet.filter (.isFinal) matching |> AutoSet.size) > 0 then
                                      ["final"]
                                    else
                                      ["non-final"]
                                  )
                              )
                          )
                      , span
                          [ HA.class "computation-progress-to-do" ]
                          [ viewInputProcessing datum.remainingData [] ]
                      ]
                  )
                  maybeDatum
                |> Maybe.withDefault (text "")
              ]
    ]

viewInputProcessing : List Char -> List String -> Html msg
viewInputProcessing characters classes =
  span
    [ HA.classList <| List.map (\v -> (v, True)) classes ]
    [ text <| String.fromList characters ]

viewAddTestPanelContent : Model -> Html Msg
viewAddTestPanelContent model =
  let
    test =
      AutoDict.get model.fdg_model.currentPackage.currentTestKey model.fdg_model.currentPackage.tests
    testInput =
      Maybe.map .input test 
      |> Maybe.withDefault ""
    testExpected =
      Maybe.map .expectation test
      |> Maybe.withDefault ExpectAccepted
    testExpectationElement =
      case testInput of
        "" ->
          text ""
        _ ->
          div
            [ HA.css
                [ Css.whiteSpace Css.preWrap
                , Css.padding4 (Css.px 0) (Css.px 0) (Css.px 15) (Css.px 15)
                , Css.userSelect Css.none
                ]
            ]
            [ span
                []
                [ text "When this input is received, the computation should " ]
            -- , span
            --     [ HA.class "button"
            --     , HA.css
            --         [ Css.backgroundColor <| Css.rgb 0x62 0x72 0xa4 -- --dracula-comment
            --         ]
            --     , onClick <| UpdateTestPanelContent testInput <|
            --         if testExpected == ExpectAccepted then ExpectRejected else ExpectAccepted
            --     ]
            --     [ text "⇄"
            --     ]
            -- , text " "
            , span
                [ HA.classList
                    [ ("test_panel__accept-text", testExpected == ExpectAccepted)
                    , ("test_panel__reject-text", testExpected == ExpectRejected)
                    ]
                , HA.css
                    [ Css.fontWeight Css.bold
                    , Css.textDecorationLine Css.underline
                    , Css.textDecorationStyle Css.dashed
                    , Css.cursor Css.pointer
                    , Css.color <|
                        if testExpected == ExpectAccepted then
                          Css.rgb 0x50 0xfa 0x7b -- --dracula-green
                        else
                          Css.rgb 0xff 0x79 0xc6 -- --dracula-pink
                    ]
                , onClick <| UpdateTestPanelContent testInput <|
                    if testExpected == ExpectAccepted then ExpectRejected else ExpectAccepted
                ]
                [ text
                    ( case testExpected of
                        ExpectAccepted -> "accept"
                        ExpectRejected -> "reject"
                    )
                ]
            , span
                [ HA.css
                    [ Css.whiteSpace Css.preWrap
                    ]
                ]
                [ text " it." ]
            ]
    testContentArea =
      textarea 
        [ HA.class "right-bottom-panel__textarea"
        , HA.value testInput
        , onInput (\v -> UpdateTestPanelContent v testExpected)
        , HA.placeholder "Enter your test input here"
        , HA.disabled <| Maybe.Extra.isJust model.fdg_model.currentOperation
        ]
        []
  in
    case model.executionStage of
      Ready ->
        div
          [ HA.class "bottom-panel__content"]
          [ testExpectationElement
          , testContentArea
          ]

      NotReady ->
        div 
          [ HA.class "notready-output" ]
          [ p [ HA.class "output-line" ] [ text "All changes must be committed or undone before you can execute." ] ]
      
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
    , HA.value (model.fdg_model.currentPackage.description |> Maybe.withDefault "")
    , onInput UpdateDescriptionPanelContent
    , HA.placeholder "What does this computation do?"
    , HA.disabled <| Maybe.Extra.isJust model.fdg_model.currentOperation
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
        [ text (getBottomPanelTitle model.selectedBottomPanel model.executionStage) ]
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

getBottomPanelTitle : BottomPanel -> ExecutionStage -> String
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

getActionButtonClass : ExecutionStage -> Msg -> String
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
    [ span [] [ text (getStatusMessage model.executionStage) ]
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

getStatusMessage : ExecutionStage -> String
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
        model.fdg_model
      |> Sub.map ForceDirectedMsg
    , BE.onResize (\w h -> OnResize (toFloat w, toFloat h) {- |> Debug.log "Raw resize values" -})
    , if model.isDraggingHorizontalSplitter || model.isDraggingVerticalSplitter then
        Sub.batch
          [ BE.onMouseMove (D.map2 MouseMove (D.field "clientX" D.float) (D.field "clientY" D.float))
          , BE.onMouseUp (D.succeed StopDragging)
          ]
      else
        Sub.none
    ]