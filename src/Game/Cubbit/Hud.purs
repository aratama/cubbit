module Game.Cubbit.Hud (Query(..), HudDriver, HudEffects, initializeHud, queryToHud) where

import Control.Alt (void)
import Control.Alternative (when)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import DOM.Event.Event (Event, preventDefault, stopPropagation)
import DOM.Event.Types (mouseEventToEvent)
import DOM.HTML.Types (HTMLElement)
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Ord (max, min)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex, runBlockIndex)
import Game.Cubbit.PointerLock (exitPointerLock, requestPointerLock)
import Game.Cubbit.Types (Mode(..), State(..), Options)
import Game.Cubbit.Vec (Vec)
import Halogen (Component, ComponentDSL, ComponentHTML, HalogenEffects, HalogenIO, component, liftEff, modify)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML (ClassName(ClassName), HTML, PropName(PropName), button, div, img, p, prop, text)
import Halogen.HTML.Elements (canvas)
import Halogen.HTML.Events (onClick, onContextMenu)
import Halogen.HTML.Properties (LengthLiteral(..), I, IProp, class_, height, id_, src, width)
import Halogen.Query (action, get)
import Halogen.VirtualDOM.Driver (runUI)
import Math (pi)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), bind, pure, ($), (<>), show, (/=), (+), (*), negate, (-))
import Unsafe.Coerce (unsafeCoerce)

type HudState = { ref :: Ref State, cursorPosition :: BlockIndex, options :: Options }

initialState :: Ref State -> Options -> HudState
initialState ref options = { ref, cursorPosition: blockIndex 0 0 0, options }

data Query a = SetCursorPosition BlockIndex a
             | PreventDefault Event a
             | SetMode Mode a
             | SetPosition Vec a
             | TogglePointerLock a

type HudEffects eff = HalogenEffects (ajax :: AJAX | eff)

render :: HudState -> ComponentHTML Query
render state = div [id_ "content", class_ (ClassName "content-layer"), onContextMenu (\e -> Just (action (PreventDefault (mouseEventToEvent e)))) ] [
    canvas [id_ "renderCanvas"],
    canvas [id_ "canvas2d", width $ Pixels 1280, height $ Pixels 720],
    img [src "screenshade.png", styleStr "pointer-events:none; display:block; position:absolute; left:0; top:0; width:100%; height: 100%;"],

    p [id_ "message-box-top"] [],
    p [id_ "message-box"] [text $ "Cubbit×Cubbit Playable Demo"],
    div [id_ "buttons"] [
        button [id_ "move", onClick \e -> Just (SetMode Move unit)] [text "Move"],
        button [id_ "add", onClick \e -> Just (SetMode Put unit)] [text "Add"],
        button [id_ "remove", onClick \e -> Just (SetMode Remove unit)] [text "Remove"],
        button [id_ "position", onClick \e -> Just (SetPosition { x: 0.0, y: 30.0, z: 0.0 } unit)] [text "Init Pos"],
        button [id_ "first-person-view", onClick \e -> Just (TogglePointerLock unit)] [text "Fst Person View"],
        button [id_ "debuglayer"] [text "DebugLayer"]
    ],
    div [id_ "cursor-position"] [text $ "cursor: (" <> show index.x <> ", " <> show index.y <> ", " <> show index.z <> ")"]
]

  where

    index = runBlockIndex state.cursorPosition

styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (PropName "style") Nothing value)

eval :: forall eff. Query ~> ComponentDSL HudState Query Void (Aff (HudEffects eff))
eval = case _ of

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
        state <- get
        liftEff (modifyRef state.ref (\(State s) -> State s { mode = mode }))
        pure next

    (SetPosition position next) -> do
        state <- get
        liftEff (modifyRef state.ref (\(State s) -> State s { position = position }))
        pure next

    (TogglePointerLock next) -> do
        s <- get
        liftEff do
            modifyRef s.ref (\(State state) -> State state { firstPersonView = not state.firstPersonView })
            State state <- readRef s.ref
            let options = s.options
            if state.firstPersonView
                then requestPointerLock (\e -> do
                    modifyRef s.ref (\(State state) -> State state {
                        playerRotation = state.playerRotation + e.movementX * options.pointerHorizontalSensitivity,
                        playerPitch = max (-pi * 0.45) (min (pi * 0.45) state.playerPitch - e.movementY * options.pointerVerticalSensitivity)
                    })
                    pure unit
                ) (modifyRef s.ref (\(State state) -> State state {
                    firstPersonView = false
                }))
                else exitPointerLock
        pure next

ui :: forall eff. Ref State -> Options -> Component HTML Query Void (Aff (HudEffects eff))
ui ref options = component { render, eval, initialState: initialState ref options }



type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))

initializeHud :: forall eff. Ref State -> Options -> HTMLElement -> Aff (HudEffects eff) (HudDriver eff)
initializeHud ref options body = do

    runUI (ui ref options) body


queryToHud :: forall eff. HalogenIO Query Void (Aff (HudEffects (console :: CONSOLE | eff))) -> (Unit -> Query Unit) -> Eff ((HudEffects (console :: CONSOLE | eff))) Unit
queryToHud driver query = void $ runAff logShow (\_ -> pure unit) (driver.query (query unit))
