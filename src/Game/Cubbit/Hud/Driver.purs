--module Game.Cubbit.Hud.Driver (HudDriver, initializeHud, queryToHud, peekState) where
module Game.Cubbit.Hud.Driver (HudDriver, initializeHud, queryToHud) where

import Control.Alt (void)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (Ref)
import DOM.HTML.Types (HTMLElement)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Config (Config(Config))
import Game.Cubbit.Hud.Eval (eval)
import Game.Cubbit.Hud.Render (render)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.Option (Options)
import Game.Cubbit.Sounds (Sounds)
import Game.Cubbit.Types (State(..))
import Graphics.Babylon.Types (Mesh, Scene)
import Halogen (Component, HalogenIO, component)
import Halogen.HTML (HTML)
import Halogen.VirtualDOM.Driver (runUI)
import Prelude (pure, ($))

ui :: forall eff. State -> Ref State -> Options -> Scene -> Mesh -> Materials -> Sounds-> Boolean -> Component HTML Query Void (Aff (HudEffects eff))
ui initialState ref options scene cursor materials sounds mute = component {
    render,
    eval: eval scene cursor materials options ref sounds,
    initialState: initialState
}

type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))

initializeHud :: forall eff. State -> Ref State -> Options -> HTMLElement -> Scene -> Mesh -> Materials -> Sounds -> Aff (HudEffects eff) (HudDriver eff)
initializeHud (State state@{ config: Config config }) ref options body scene cursor materials sounds = do
    runUI (ui (State state) ref options scene cursor materials sounds config.mute) body

queryToHud :: forall eff. HalogenIO Query Void (Aff (HudEffects eff)) -> (Unit -> Query Unit) -> Eff (HudEffects eff) Unit
queryToHud driver query = void $ runAff logShow (\_ -> pure unit) (driver.query (query unit))




