module Game.Cubbit.Hud (Query(..), HudDriver, HudEffects, initializeHud, queryToHud) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef)
import DOM.Event.Event (Event, preventDefault, stopPropagation)
import DOM.Event.KeyboardEvent (KeyboardEvent, key, keyboardEventToEvent)
import DOM.Event.MouseEvent (MouseEvent, buttons)
import DOM.Event.Types (EventType(..), mouseEventToEvent)
import DOM.HTML.Types (HTMLElement)
import DOM.WebStorage (STORAGE)
import Data.Array (replicate)
import Data.BooleanAlgebra (not)
import Data.Functor (map)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (max, min)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex, runBlockIndex)
import Game.Cubbit.BlockType (airBlock, dirtBlock, grassBlock, leavesBlock, waterBlock, woodBlock)
import Game.Cubbit.Config (Config(..), readConfig, writeConfig)
import Game.Cubbit.Option (Options)
import Game.Cubbit.Control (pickBlock)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Types (Materials, Mode(..), Sounds, State(..))
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
import Graphics.Babylon.Sound (play, setVolume)
import Graphics.Babylon.Types (BABYLON, Mesh, Scene, Sound)
import Halogen (Component, ComponentDSL, ComponentHTML, HalogenEffects, HalogenIO, component, liftEff, modify)
import Halogen.HTML (ClassName(ClassName), HTML, PropName(PropName), div, img, p, prop, text)
import Halogen.HTML.Elements (canvas, i)
import Halogen.HTML.Events (handler, onClick, onContextMenu, onKeyDown, onKeyUp, onMouseDown, onMouseMove)
import Halogen.HTML.Properties (I, IProp, LengthLiteral(..), autofocus, class_, height, id_, src, width, tabIndex)
import Halogen.Query (action, get)
import Halogen.VirtualDOM.Driver (runUI)
import Math (pi)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), bind, negate, otherwise, pure, show, ($), (*), (+), (-), (/=), (<<<), (<>), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

type HudState = {
    cursorPosition :: BlockIndex,
    mode :: Mode,
    mute :: Boolean,
    centerPanelVisible :: Boolean
}

initialState :: Boolean -> HudState
initialState mute = {
    cursorPosition: blockIndex 0 0 0,
    mode : Move,
    mute: mute,
    centerPanelVisible: false
}

data Query a = SetCursorPosition BlockIndex a
             | PreventDefault Event a
             | SetMode Mode a
             | SetPosition Vec a
             | TogglePointerLock a
             | SetMousePosition MouseEvent a
             | OnMouseClick MouseEvent a
             | ToggleDebugLayer a
             | Zoom MouseEvent a
             | OnKeyDown KeyboardEvent a
             | OnKeyUp KeyboardEvent a
             | ToggleMute a
             | SetCenterPanelVisible Boolean a
             | Nop Event a

type HudEffects eff = HalogenEffects (ajax :: AJAX, babylon :: BABYLON, storage :: STORAGE | eff)

slotClass :: forall r i. Boolean -> IProp (class :: I | r) i
slotClass active = class_ (ClassName ("slot" <> if active then " active" else ""))

icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []


render :: HudState -> ComponentHTML Query
render state = div [
    id_ "content",
    class_ (ClassName "content-layer"),
    onContextMenu (\e -> Just (action (PreventDefault (mouseEventToEvent e)))),
    tabIndex 0,
    unsafeCoerce (autofocus true),
    onKeyDown \e -> Just (OnKeyDown e unit),
    onKeyUp \e -> Just (OnKeyUp e unit),
    onMouseMove \e -> Just (SetMousePosition e unit),
    onMouseDown \e -> Just (OnMouseClick e unit),
    onWheel \e -> Just (Zoom e unit)
] [
    canvas [
        id_ "canvas2d",
        width $ Pixels 1280,
        height $ Pixels 720
    ],
    img [id_ "screen-shade", src "screenshade.png"],

    div [id_ "cursor-position"] [text $ "cursor: (" <> show index.x <> ", " <> show index.y <> ", " <> show index.z <> ")"],

    div [id_ "life"] (replicate 12 (div [class_ (ClassName "active")] [icon "heart"]) <> replicate 2 (icon "heart")),


    p [id_ "message-box-top"] [],
    p [id_ "message-box"] [text $ "Cubbit×Cubbit Playable Demo"],
    div [
        id_ "right-panel",
        suppressMouseMove,
        suppressMouseDown
    ] [
        div [class_ (ClassName "button first-person-view"), onClick \e -> Just (TogglePointerLock unit)] [icon "eye"],
        div [class_ (ClassName "button initialize-position"), onClick \e -> Just (SetPosition { x: 0.0, y: 30.0, z: 0.0 } unit)] [icon "plane"],
        div [class_ (ClassName "button mute"), onClick \e -> Just (ToggleMute unit)] [icon if state.mute then "volume-off" else "volume-up"],
        div [class_ (ClassName "button initialize-position"), onClick \e -> Just (ToggleDebugLayer unit)] [icon "gear"]
    ],


    div [id_ "open-center-panel", onClick \e -> Just (SetCenterPanelVisible true unit)] [icon "suitcase"],

    if state.centerPanelVisible
        then div [
                id_ "center-panel-outer",
                onClick \e -> Just (SetCenterPanelVisible false unit),
                suppressMouseMove,
                suppressMouseDown
            ] [div [id_ "center-panel"] []]
        else text "",

    div [
        id_ "hotbar",
        suppressMouseMove,
        suppressMouseDown
    ] [
        div [id_ "hotbar-lower"] [],
        div [id_ "hotbar-upper"] hotbuttons
    ]
]

  where
    suppressMouseMove = onMouseMove \e -> Just (Nop (mouseEventToEvent e) unit)
    suppressMouseDown = onMouseDown \e -> Just (Nop (mouseEventToEvent e) unit)

    hotbuttons = map slot [
        Just Move,
        Just (Put grassBlock),
        Just (Put woodBlock),
        Just (Put waterBlock),
        Just (Put leavesBlock),
        Just (Put dirtBlock),
        Nothing,
        Just Remove
    ]

    tool Move = "toolicon/bow.svg"
    tool (Put t) | t == grassBlock = "toolicon/grass.svg"
                 | t == woodBlock = "toolicon/wood.svg"
                 | t == waterBlock = "toolicon/water.svg"
                 | t == leavesBlock = "toolicon/leaves.svg"
                 | t == dirtBlock = "toolicon/dirt.svg"
                 | otherwise = "toolicon/grass.svg"
    tool Remove = "toolicon/pickaxe.svg"

    slot (Just mode) = div [
        slotClass (state.mode == mode),
        onClick \e -> Just (SetMode mode unit)
    ] [img [src (tool mode)]]
    slot Nothing = div [slotClass false] []

    index = runBlockIndex state.cursorPosition

styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (PropName "style") Nothing value)

eval :: forall eff. Scene -> Mesh -> Materials -> Options -> Ref State -> Sounds -> (Query ~> ComponentDSL HudState Query Void (Aff (HudEffects eff)))
eval scene cursor materials options ref sounds = case _ of

    (PreventDefault e next) -> do
        liftEff (preventDefault e)
        liftEff (stopPropagation e)
        pure next

    (SetCursorPosition position next) -> do
        state <- get
        when (position /= state.cursorPosition) do
            modify (_ { cursorPosition = position })
        pure next

    (SetMode mode next) -> do
        liftEff do
            State s <- readRef ref
            when (s.mode /= mode) do
                writeRef ref (State s { mode = mode })
                play sounds.switchSound
        modify _ { mode = mode }
        pure next

    (SetPosition position next) -> do
        liftEff (modifyRef ref (\(State s) -> State s { position = position }))
        pure next

    (TogglePointerLock next) -> do
        liftEff do
            modifyRef ref (\(State state) -> State state { firstPersonView = not state.firstPersonView })
            State state <- readRef ref
            if state.firstPersonView
                then requestPointerLock (\e -> do
                    modifyRef ref (\(State state) -> State state {
                        playerRotation = state.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                        playerPitch = max (-pi * 0.45) (min (pi * 0.45) state.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                    })
                    pure unit
                ) (modifyRef ref (\(State state) -> State state {
                    firstPersonView = false
                }))
                else exitPointerLock
        pure next

    (SetMousePosition e next) -> do
        liftEff do
            modifyRef ref \(State state) ->
                let isRightButton = buttons e == 2
                    dx = offsetX e - state.mousePosition.x
                    dy = offsetY e - state.mousePosition.y
                        in State state {
                                mousePosition = {
                                    x: offsetX e ,
                                    y: offsetY e
                                },
                                cameraYaw = if isRightButton then state.cameraYaw + toNumber dx * options.cameraHorizontalSensitivity else state.cameraYaw,
                                cameraPitch = if isRightButton then max (-pi * 0.45) $ min (pi * 0.45) $ state.cameraPitch + toNumber dy * options.cameraVertialSensitivity else state.cameraPitch
                            }

        pure next


    (ToggleDebugLayer next) -> do
        liftEff do
            modifyRef ref (\(State state) -> State state { debugLayer = not state.debugLayer })

            State state <- readRef ref
            if state.debugLayer
                then getDebugLayer scene >>= DebugLayer.show true true Nothing
                else getDebugLayer scene >>= DebugLayer.hide
        pure next

    (OnMouseClick e next) -> do
        liftEff do

            State state <- readRef ref

            modifyRef ref \(State s) -> State s {
                mousePosition = {
                    x: offsetX e ,
                    y: offsetY e
                }
            }

            when (buttons e == 1) do

                let put block = do
                        picked <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                        case picked of
                            Nothing -> pure unit
                            Just blockIndex -> editBlock ref materials scene blockIndex block

                case state.mode of
                    Put blockType -> do
                        put blockType
                        play sounds.putSound
                    Remove -> do
                        put airBlock
                        play sounds.pickSound
                    Move -> pure unit

        pure next

    (Zoom e next) -> do
        liftEff do
            modifyRef ref \(State state) -> State state {
                cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (state.cameraRange + (toNumber (deltaY e) * options.cameraZoomSpeed)))
            }
        pure next


    (OnKeyDown e next) -> do
        let go f = void do modifyRef ref \(State state) -> State (f state true)
        liftEff do
            case key e of
                " " -> go _ { spaceKey = _ }
                "w" -> go _ { wKey = _ }
                "s" -> go _ { sKey = _ }
                "a" -> go _ { aKey = _ }
                "d" -> go _ { dKey = _ }
                "r" -> go _ { rKey = _ }
                "f" -> go _ { fKey = _ }
                "q" -> go _ { qKey = _ }
                "e" -> go _ { eKey = _ }
                "t" -> go _ { tKey = _ }
                "g" -> go _ { gKey = _ }
                _ -> pure unit
            preventDefault (keyboardEventToEvent e)
            stopPropagation (keyboardEventToEvent e)

        pure next

    (OnKeyUp e next) -> do
        let go f = void do modifyRef ref \(State state) -> State (f state false)
        liftEff do
            case key e of
                " " -> go _ { spaceKey = _ }
                "w" -> go _ { wKey = _ }
                "s" -> go _ { sKey = _ }
                "a" -> go _ { aKey = _ }
                "d" -> go _ { dKey = _ }
                "r" -> go _ { rKey = _ }
                "f" -> go _ { fKey = _ }
                "q" -> go _ { qKey = _ }
                "e" -> go _ { eKey = _ }
                "t" -> go _ { tKey = _ }
                "g" -> go _ { gKey = _ }
                _ -> pure unit
            preventDefault (keyboardEventToEvent e)
            stopPropagation (keyboardEventToEvent e)
        pure next

    (ToggleMute next) -> do
        modify \state -> state { mute = not state.mute }
        state <- get
        liftEff do
            setMute state.mute sounds
            writeConfig (Config { mute: state.mute })
        pure next

    (SetCenterPanelVisible visible next) -> do
        modify \state -> state { centerPanelVisible = visible }
        pure next

    (Nop e next) -> do
        liftEff do
            preventDefault e
            stopPropagation e
        pure next

setMute :: forall eff. Boolean -> Sounds -> Eff (babylon :: BABYLON | eff) Unit
setMute mute sounds = do
    let go = setVolume (if mute then 0.0 else 1.0)
    go sounds.forestSound
    go sounds.switchSound
    go sounds.pickSound
    go sounds.putSound


ui :: forall eff. Ref State -> Options -> Scene -> Mesh -> Materials -> Sounds-> Boolean -> Component HTML Query Void (Aff (HudEffects eff))
ui ref options scene cursor materials sounds mute = component {
    render,
    eval: eval scene cursor materials options ref sounds,
    initialState: initialState mute
}

type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))

initializeHud :: forall eff. Ref State -> Options -> HTMLElement -> Scene -> Mesh -> Materials -> Sounds -> Aff (HudEffects eff) (HudDriver eff)
initializeHud ref options body scene cursor materials sounds = do
    Config config <- liftEff $ readConfig
    liftEff $ setMute config.mute sounds
    runUI (ui ref options scene cursor materials sounds config.mute) body

queryToHud :: forall eff. HalogenIO Query Void (Aff (HudEffects (console :: CONSOLE | eff))) -> (Unit -> Query Unit) -> Eff ((HudEffects (console :: CONSOLE | eff))) Unit
queryToHud driver query = void $ runAff logShow (\_ -> pure unit) (driver.query (query unit))

offsetX :: MouseEvent -> Int
offsetX e = (unsafeCoerce e).offsetX

offsetY :: MouseEvent -> Int
offsetY e = (unsafeCoerce e).offsetY

deltaY :: MouseEvent -> Int
deltaY e = (unsafeCoerce e).deltaY

onWheel :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseDown :: I | r) i
onWheel = handler (EventType "wheel") <<< mouseHandler

mouseHandler :: forall i. (MouseEvent -> Maybe i) -> Event -> Maybe i
mouseHandler = unsafeCoerce

