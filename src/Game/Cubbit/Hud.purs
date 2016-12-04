module Game.Cubbit.Hud (Query(..), initializeHud) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Ref (Ref)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault, stopPropagation)
import DOM.Event.Types (mouseEventToEvent)
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex)
import Game.Cubbit.Types (State)
import Halogen (Component, ComponentDSL, ComponentHTML, HalogenEffects, HalogenIO, component, liftEff, modify)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML (ClassName(ClassName), HTML, PropName(PropName), button, div, img, p, prop, text)
import Halogen.HTML.Elements (canvas)
import Halogen.HTML.Events (onContextMenu)
import Halogen.HTML.Properties (LengthLiteral(..), I, IProp, class_, height, id_, src, width)
import Halogen.Query (action)
import Halogen.VirtualDOM.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), bind, pure, ($))
import Unsafe.Coerce (unsafeCoerce)

type HudState = { cursorPosition :: BlockIndex }

initialState :: HudState
initialState = { cursorPosition: blockIndex 0 0 0 }

data Query a = SetCursorPosition BlockIndex a
             | PreventDefault Event a

type Effects eff = (dom :: DOM, avar :: AVAR, ajax :: AJAX | eff)

render :: HudState -> ComponentHTML Query
render state = div [id_ "content", class_ (ClassName "content-layer"), onContextMenu (\e -> Just (action (PreventDefault (mouseEventToEvent e)))) ] [
    canvas [id_ "renderCanvas"],
    canvas [id_ "canvas2d", width $ Pixels 1280, height $ Pixels 720],
    img [src "screenshade.png", styleStr "pointer-events:none; display:block; position:absolute; left:0; top:0; width:100%; height: 100%;"],

    p [id_ "message-box-top"] [],
    p [id_ "message-box"] [text $ "Cubbit×Cubbit Playable Demo"],
    div [id_ "buttons"] [
        button [id_ "move"] [text "Move"],
        button [id_ "add"] [text "Add"],
        button [id_ "remove"] [text "Remove"],
        button [id_ "position"] [text "Init Pos"],
        button [id_ "minimap"] [text "Minimap"],
        button [id_ "first-person-view"] [text "Fst Person View"],
        button [id_ "debuglayer"] [text "DebugLayer"]
    ],
    div [id_ "cursor-position"] []
]

styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (PropName "style") Nothing value)

eval :: forall eff. Query ~> ComponentDSL HudState Query Void (Aff (Effects eff))
eval = case _ of
    (PreventDefault e next) -> do
        liftEff (preventDefault e)
        liftEff (stopPropagation e)
        pure next

    (SetCursorPosition position next) -> do
        modify (\state -> state { cursorPosition = position })
        pure next

ui :: forall eff. Component HTML Query Void (Aff (Effects eff))
ui = component { render, eval, initialState }

type E eff = HalogenEffects (ajax :: AJAX | eff)

initializeHud :: forall eff. Ref State -> Aff (E eff) (HalogenIO Query Void (Aff (E eff)))
initializeHud ref = do
    body <- awaitBody
    runUI ui body
