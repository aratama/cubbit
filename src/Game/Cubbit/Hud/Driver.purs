--module Game.Cubbit.Hud.Driver (HudDriver, initializeHud, queryToHud, peekState) where
module Game.Cubbit.Hud.Driver (HudDriver, initializeHud, queryToHud) where

import Control.Alt (void)
import Control.Category (id)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (Ref)
import DOM.HTML.Types (HTMLElement)
import Data.Maybe (Maybe(Nothing))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (blockIndex)
import Game.Cubbit.Config (Config(Config), readConfig)
import Game.Cubbit.Hud.Eval (eval, setMute)
import Game.Cubbit.Hud.Render (render)
import Game.Cubbit.Hud.Type (HudEffects, Query(PeekState))
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.Option (Options)
import Game.Cubbit.Sounds (Sounds)
import Game.Cubbit.Types (Mode(Move), State)
import Graphics.Babylon.Types (AbstractMesh, Mesh, Scene)
import Halogen (Component, HalogenIO, component, liftEff)
import Halogen.HTML (HTML)
import Halogen.Query (request)
import Halogen.VirtualDOM.Driver (runUI)
import Prelude (bind, pure, ($), type (~>))
{-
initialState :: Boolean -> HudState
initialState mute = {
    cursorPosition: blockIndex 0 0 0,
    mode : Move,
    mute: mute,
    centerPanelVisible: false,
    nextScene: Nothing,
    gameScene: TitleScene,
    life: 10,
    maxLife: 12
}
-}

ui :: forall eff. Array AbstractMesh -> State -> Ref State -> Options -> Scene -> Mesh -> Materials -> Sounds-> Boolean -> Component HTML Query Void (Aff (HudEffects eff))
ui playerMeshes initialState ref options scene cursor materials sounds mute = component {
    render,
    eval: eval playerMeshes scene cursor materials options ref sounds,
    --initialState: initialState mute
    initialState: initialState
}

type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))

initializeHud :: forall eff. Array AbstractMesh -> State -> Ref State -> Options -> HTMLElement -> Scene -> Mesh -> Materials -> Sounds -> Aff (HudEffects eff) (HudDriver eff)
initializeHud playerMeshes initialState ref options body scene cursor materials sounds = do
    Config config <- liftEff $ readConfig
    liftEff $ setMute config.mute sounds
    runUI (ui playerMeshes initialState ref options scene cursor materials sounds config.mute) body

queryToHud :: forall eff. HalogenIO Query Void (Aff (HudEffects (console :: CONSOLE | eff))) -> (Unit -> Query Unit) -> Eff ((HudEffects (console :: CONSOLE | eff))) Unit
queryToHud driver query = void $ runAff logShow (\_ -> pure unit) (driver.query (query unit))

--peekState :: forall a eff. HalogenIO Query Void (Aff (HudEffects (console :: CONSOLE | eff))) -> Aff ((HudEffects (console :: CONSOLE | eff))) HudState
--peekState driver = driver.query (request PeekState)



