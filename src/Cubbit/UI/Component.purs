module Game.Cubbit.Hud.Component (runGameUI) where

import Control.Monad.Aff (Aff)
import DOM.HTML.Types (HTMLElement)
import Game.Cubbit.Hud.Eval (eval)
import Game.Cubbit.Hud.Render (render)
import Game.Cubbit.Hud.Type (HudDriver, HudEffects)
import Game.Cubbit.Types (State)
import Halogen (component)
-- import Halogen.VirtualDOM.Driver (runUI)
import Halogen.VDom.Driver (runUI)

runGameUI :: forall eff. HTMLElement -> State -> Aff (HudEffects eff) (HudDriver eff)
runGameUI body state = runUI (component {
    render,
    eval,
    initialState: state
}) body