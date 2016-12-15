module Game.Cubbit.Hud.Render (render) where

import DOM.Event.Event (Event)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.Types (EventType(..), mouseEventToEvent)
import Data.Array (replicate)
import Data.Functor (map)
import Data.Maybe (Maybe(..), isNothing)
import Data.Unit (unit)
import Game.Cubbit.BlockIndex (runBlockIndex)
import Game.Cubbit.BlockType (dirtBlock, grassBlock, leavesBlock, waterBlock, woodBlock)
import Game.Cubbit.Config (Config(..))
import Game.Cubbit.Hud.Type (PlayingSceneQuery(..), Query(..))
import Game.Cubbit.Types (Mode(Remove, Put, Move), State(..), SceneState(TitleSceneState, PlayingSceneState))
import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(ClassName), HTML, PropName(PropName), div, img, p, prop, text)
import Halogen.HTML.Elements (i)
import Halogen.HTML.Events (handler, onClick, onContextMenu, onKeyDown, onKeyUp, onMouseDown, onMouseMove)
import Halogen.HTML.Properties (I, IProp, autofocus, class_, id_, src, tabIndex)
import Halogen.HTML.Properties (key) as Properties
import Halogen.Query (action)
import Prelude (otherwise, show, ($), (-), (<<<), (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

slotClass :: forall r i. Boolean -> IProp (class :: I | r) i
slotClass active = class_ (ClassName ("slot" <> if active then " active" else ""))

icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

render :: State -> ComponentHTML Query
render (State state@{ config: Config config }) = div [
    id_ "content",
    Properties.key "root-content",
    class_ (ClassName "content-layer"),
    onContextMenu (\e -> Just (action (PreventDefault (mouseEventToEvent e)))),
    tabIndex 0,
    unsafeCoerce (autofocus true),
    onKeyDown \e -> Just (PlayingSceneQuery (OnKeyDown e) unit),
    onKeyUp \e -> Just (PlayingSceneQuery (OnKeyUp e) unit),
    onMouseMove \e -> Just (PlayingSceneQuery (SetMousePosition e) unit),
    onMouseDown \e -> Just (PlayingSceneQuery (OnMouseClick e) unit),
    onWheel \e -> Just (PlayingSceneQuery (Zoom e) unit)
] [

    div [Properties.key "content-inner"] case state.sceneState of

        TitleSceneState -> [
            img [
                class_ (ClassName "content-layer"),
                src "title.png",
                onClick \e -> Just (Start unit)
            ]
        ]

        PlayingSceneState playingSceneState -> let index = runBlockIndex playingSceneState.cursorPosition in [
            img [id_ "screen-shade", class_ (ClassName "content-layer"), src "screenshade.png"],

            div [id_ "cursor-position"] [text $ "cursor: (" <> show index.x <> ", " <> show index.y <> ", " <> show index.z <> ")"],

            div [id_ "life"] (replicate playingSceneState.life (div [class_ (ClassName "active")] [icon "heart"]) <> replicate (playingSceneState.maxLife - playingSceneState.life) (icon "heart")),


            p [id_ "message-box-top"] [],
            p [id_ "message-box"] [text $ "Cubbit×Cubbit Playable Demo"],
            div [
                id_ "right-panel",
                suppressMouseMove,
                suppressMouseDown
            ] [
                div [class_ (ClassName "button first-person-view"), onClick \e -> Just (PlayingSceneQuery TogglePointerLock unit)] [icon "eye"],
                div [class_ (ClassName "button initialize-position"), onClick \e -> Just (PlayingSceneQuery (SetPosition { x: 0.0, y: 30.0, z: 0.0 }) unit)] [icon "plane"],
                div [class_ (ClassName "button mute"), onClick \e -> Just (ToggleMute unit)] [icon if config.mute then "volume-off" else "volume-up"],
                div [class_ (ClassName "button initialize-position"), onClick \e -> Just (PlayingSceneQuery ToggleDebugLayer unit)] [icon "gear"],
                div [class_ (ClassName "button home"), onClick \e -> Just (PlayingSceneQuery Home unit)] [icon "home"]
            ],


            if playingSceneState.centerPanelVisible
                then div [
                        id_ "center-panel-outer",
                        onClick \e -> Just (PlayingSceneQuery (SetCenterPanelVisible false) unit),
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
                div [id_ "hotbar-upper"] (hotbuttons playingSceneState)
            ],

            div [id_ "open-center-panel", onClick \e -> Just (PlayingSceneQuery (SetCenterPanelVisible true) unit)] [icon "suitcase"]
        ],
    div [
        id_ "shadow",
        class_ (ClassName "content-layer"),
        Properties.key "shadow",
        styleStr ("opacity: " <> if isNothing state.nextScene then "0.0" else "1.0")
    ] []
]

  where
    suppressMouseMove = onMouseMove \e -> Just (Nop (mouseEventToEvent e) unit)
    suppressMouseDown = onMouseDown \e -> Just (Nop (mouseEventToEvent e) unit)


    hotbuttons playingSceneState = map slot [
        Just Move,
        Just (Put grassBlock),
        Just (Put woodBlock),
        Just (Put waterBlock),
        Just (Put leavesBlock),
        Just (Put dirtBlock),
        Nothing,
        Just Remove
    ]

      where

        tool Move = "toolicon/bow.svg"
        tool (Put t) | t == grassBlock = "toolicon/grass.svg"
                     | t == woodBlock = "toolicon/wood.svg"
                     | t == waterBlock = "toolicon/water.svg"
                     | t == leavesBlock = "toolicon/leaves.svg"
                     | t == dirtBlock = "toolicon/dirt.svg"
                     | otherwise = "toolicon/grass.svg"
        tool Remove = "toolicon/pickaxe.svg"

        slot (Just mode) = div [
            slotClass (playingSceneState.mode == mode),
            onClick \e -> Just (PlayingSceneQuery (SetMode mode) unit)
        ] [img [src (tool mode)]]
        slot Nothing = div [slotClass false] []




styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (PropName "style") Nothing value)


onWheel :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseDown :: I | r) i
onWheel = handler (EventType "wheel") <<< mouseHandler

mouseHandler :: forall i. (MouseEvent -> Maybe i) -> Event -> Maybe i
mouseHandler = unsafeCoerce

