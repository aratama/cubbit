module Game.Cubbit.Hud.Driver (initializeHud, queryToHud) where

import Control.Alt (void)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import DOM.HTML.Types (HTMLElement)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Config (Config(Config))
import Game.Cubbit.Hud.Eval (eval)
import Game.Cubbit.Hud.Render (render)
import Game.Cubbit.Hud.Type (HudEffects, Query, HudDriver)
import Game.Cubbit.Types (State(..))
import Halogen (Component, HalogenIO, component)
import Halogen.HTML (HTML)
-- import Halogen.VirtualDOM.Driver (runUI)
import Halogen.VDom.Driver (runUI)
import Prelude (pure, ($))

ui :: forall eff. State -> Boolean -> Component HTML Query Void (Aff (HudEffects eff))
ui initialState mute = component {
    render,
    eval,
    initialState: initialState
}

initializeHud :: forall eff. State -> HTMLElement -> Aff (HudEffects eff) (HudDriver eff)
initializeHud (State state@{ config: Config config }) body = do
    runUI (ui (State state) config.mute) body

queryToHud :: forall eff. HalogenIO Query Void (Aff (HudEffects eff)) -> (Unit -> Query Unit) -> Eff (HudEffects eff) Unit
queryToHud driver query = void $ runAff logShow (\_ -> pure unit) (driver.query (query unit))




