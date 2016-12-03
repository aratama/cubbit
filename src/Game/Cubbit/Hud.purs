module Game.Cubbit.Hud (Query(..), initializeHud) where

import Control.Monad.Aff (Aff)
import DOM.HTML.Event.EventTypes (contextmenu)
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, ComponentHTML, HalogenEffects, component, modify, runUI)
import Halogen.Driver (Driver)
import Halogen.HTML.Elements.Indexed (canvas)
import Halogen.HTML.Events (preventDefault, stopPropagation)
import Halogen.HTML.Events.Indexed (input_, onContextMenu)
import Halogen.HTML.Indexed (button, className, div, div_, img, p, prop, propName, text)
import Halogen.HTML.Properties (pixels)
import Halogen.HTML.Properties.Indexed (I, IProp, class_, height, id_, src, width)
import Halogen.Query (action)
import Halogen.Util (awaitBody)
import Prelude (type (~>), bind, not, pure, ($), ($>))
import Unsafe.Coerce (unsafeCoerce)

data Query a = ToggleState a

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (propName "style") Nothing value)

ui :: forall g. Component State Query g
ui = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render state = div [id_ "content", class_ (className "content-layer"), onContextMenu (\_ -> preventDefault $> stopPropagation $> Just (action ToggleState))] [
            canvas [id_ "renderCanvas"],
            canvas [id_ "canvas2d", width $ pixels 1280, height $ pixels 720],
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

        eval :: Query ~> ComponentDSL State Query g
        eval (ToggleState next) = do
            modify (\state -> { on: not state.on })
            pure next

initializeHud :: forall eff. Aff (HalogenEffects eff) (Driver Query eff)
initializeHud = do
    body <- awaitBody
    runUI ui initialState body
