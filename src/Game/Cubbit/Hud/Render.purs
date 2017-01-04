module Game.Cubbit.Hud.Render (render) where

import DOM.Event.Event (Event)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.Types (EventType(..), mouseEventToEvent)
import DOM.Event.WheelEvent (WheelEvent)
import Data.Array (replicate, (..))
import Data.Functor (map, mapFlipped)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (runBlockIndex)
import Game.Cubbit.BlockType (dirtBlock, grassBlock, leavesBlock, waterBlock, woodBlock)
import Game.Cubbit.Captions (Language(Ja, En), bgmVolume, chunkArea, clickToStart, graphics, language, modeSelection, waterMaterial, multiplayerOnlineMode, mute, off, on, seVolume, shadow, shadowArea, singleplayerOfflineMode, sounds, terrain, vertexColor)
import Game.Cubbit.Config (Config(..))
import Game.Cubbit.Constants (sliderMaxValue)
import Game.Cubbit.Hud.Type (PlayingSceneQuery(..), Query(..), QueryA(..), getRes)
import Game.Cubbit.Resources (resourceCount)
import Game.Cubbit.Types (GameMode(..), Mode(..), SceneState(..), State(..))
import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(ClassName), HTML, PropName(PropName), div, h1, h2, img, p, prop, text)
import Halogen.HTML.Elements (a, button, canvas, i, p_)
import Halogen.HTML.Events (handler, onClick, onContextMenu, onKeyDown, onKeyUp, onMouseDown, onMouseMove)
import Halogen.HTML.Properties (I, IProp, autofocus, class_, href, id_, src, tabIndex, target)
import Halogen.HTML.Properties (key) as Properties
import Prelude (otherwise, show, ($), (-), (<<<), (<>), (==), (<=))
import Unsafe.Coerce (unsafeCoerce)

slotClass :: forall r i. Boolean -> IProp (class :: I | r) i
slotClass active = class_ (ClassName ("slot" <> if active then " active" else ""))

icon :: forall p i. String -> HTML p i
icon name = i [class_ (ClassName ("fa fa-" <> name))] []

send :: PlayingSceneQuery -> Maybe (Query Unit)
send action = Just (Query (PlayingSceneQuery action) unit)

send' :: QueryA -> Maybe (Query Unit)
send' q = Just (Query q unit)

render :: State -> ComponentHTML Query
render (State state@{ config: Config config }) = div [
    id_ "content",
    Properties.key "root-content",
    class_ (ClassName "content-layer"),
    onContextMenu \e ->send' (PreventDefault (mouseEventToEvent e)),
    tabIndex 0,
    unsafeCoerce (autofocus true),
    onKeyDown \e -> send (OnKeyDown e),
    onKeyUp \e -> send (OnKeyUp e),
    onMouseMove \e -> send (SetMousePosition e),
    onMouseDown \e -> send (OnMouseClick e),
    onWheel \e -> send (Zoom e)
] [

    canvas [id_ "renderCanvas", class_ (ClassName "content-layer")],

    div [Properties.key "content-inner"] case state.sceneState of

        LoadingSceneState progress -> [
            img [class_ (ClassName "content-layer"), src "image/loading.png"],
            div [class_ (ClassName "progress")] $ mapFlipped (0 .. resourceCount) \i ->
                div [class_ (ClassName ("cell" <> if i <= progress then " fill" else ""))] []
        ]

        TitleSceneState titleSceneState -> [
            img [
                class_ (ClassName "content-layer"),
                src "image/title.png",
                Properties.key "image/title.png",
                onClick \e -> send' (ModeSelect titleSceneState.res)
            ],

            div [
                class_ (ClassName "show-config"),
                onClick \e -> send' (ShowConfig titleSceneState.res)
            ] [icon "gear"],

            div [class_ (ClassName "click-to-start")] [mtext clickToStart]
        ]

        ModeSelectionSceneState { res } -> [
            div [class_ (ClassName "content-layer mode-root")] [

                h1 [] [icon "tree", mtext modeSelection],
                div [class_ (ClassName "home button"), onClick \e -> send' (Home res)] [icon "home"],
                div [class_ (ClassName "singleplayer mode button"), onClick \e -> send' $ Start SinglePlayerMode res] [icon "user", mtext singleplayerOfflineMode],
                div [class_ (ClassName "multiplayer mode button"), onClick \e -> send' $ Start MultiplayerMode res] [icon "users", mtext multiplayerOnlineMode]
            ]
        ]

        PlayingSceneState playingSceneState -> let index = runBlockIndex playingSceneState.cursorPosition in [
            img [
                Properties.key "image/screenshade.png",
                id_ "screen-shade",
                class_ (ClassName "content-layer"),
                src "image/screenshade.png"
            ],

            div [id_ "cursor-position"] [text $ "cursor: (" <> show index.x <> ", " <> show index.y <> ", " <> show index.z <> ")"],

            div [id_ "life"] (replicate playingSceneState.life (div [class_ (ClassName "active")] [icon "heart"]) <> replicate (playingSceneState.maxLife - playingSceneState.life) (icon "heart")),


            p [id_ "message-box-top"] [],
            p [id_ "message-box"] [text $ "Cubbit×Cubbit Playable Demo"],
            div [
                id_ "right-panel",
                suppressMouseMove,
                suppressMouseDown
            ] [
                div [class_ (ClassName "button"), onClick \e -> send TogglePointerLock] [icon "eye"],
                div [class_ (ClassName "button"), onClick \e -> send (SetPosition { x: 0.0, y: 30.0, z: 0.0 })] [icon "plane"],
                div [class_ (ClassName "button"), onClick \e -> send' (ShowConfig playingSceneState.res)] [icon "gear"],
                div [class_ (ClassName "button"), onClick \e -> send' (Home playingSceneState.res)] [icon "home"]
            ],


            if playingSceneState.centerPanelVisible
                then div [
                        id_ "center-panel-outer",
                        onClick \e -> send (SetCenterPanelVisible false),
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

            div [id_ "open-center-panel", onClick \e -> send (SetCenterPanelVisible true)] [icon "suitcase"]
        ],

    case getRes (State state) of
        Nothing -> text ""
        Just res -> div [
                Properties.key "config-root",
                class_ (ClassName ("content-layer config-root" <> if state.configVisible then " visible" else "")),
                onClick \e -> send' (CloseConfig res),
                suppressMouseDown,
                suppressMouseMove
            ] [
                div [
                    class_ (ClassName "config-inner"),
                    onClick \e -> send' (Nop (mouseEventToEvent e))
                ] [
                    h2 [class_ (ClassName "config-heading")] [icon "language", mtext language],
                    languageButton En "English" res,
                    languageButton Ja "日本語" res,

                    h2 [class_ (ClassName "config-heading")] [icon "volume-up", mtext sounds],
                    option mute (toggle config.mute ToggleMute),
                    option bgmVolume (slider config.bgmVolume (SetBGMVolume res)),
                    option seVolume (slider config.seVolume (SetSEVolume res)),

                    h2 [class_ (ClassName "config-heading")] [icon "photo", mtext graphics],
                    option shadow (toggle config.shadow (ToggleShadow res)),
                    option shadowArea (slider config.shadowArea (SetShadowArea res)),
                    option vertexColor (toggle config.vertexColor (ToggleVertexColor res)),
                    option waterMaterial (toggle config.waterMaterial (ToggleWaterMaterial res)),

                    h2 [class_ (ClassName "config-heading")] [icon "cubes", mtext terrain],
                    option chunkArea (slider config.chunkArea (SetChunkArea res)),

                    p_ [a [
                        target "_blank",
                        href "LICENSE.txt",
                        onClick \e -> send' (StopPropagation (mouseEventToEvent e))
                    ] [text "License Attribution"]]
                ]
            ],

            div [
                id_ "shadow",
                class_ (ClassName ("content-layer" <> if state.nextScene then "" else " hide")),
                Properties.key "shadow"
            ] []
        ]

  where

    mtext t = text (t config.language)

    suppressMouseMove = onMouseMove \e -> send' (Nop (mouseEventToEvent e))
    suppressMouseDown = onMouseDown \e -> send' (Nop (mouseEventToEvent e))

    languageButton lang caption res = button [
        class_ $ ClassName $ "config-language" <> if config.language == lang then " active" else "",
        onClick \_ -> send' $ SetLanguage lang res
    ] [text caption]


    option caption ui = div [class_ (ClassName "config-option")] [
        div [
            class_ (ClassName "config-caption")
        ] [
            text (caption config.language)
        ],
        ui
    ]

    slider value action = div [
        class_ (ClassName "config-slider")
    ] (mapFlipped (0 .. sliderMaxValue) \i ->
        div [
            class_ (ClassName ("config-slider-box " <> if i <= value then "fill" else "empty")),
            onClick \e -> Just (Query (action i) unit)
        ] []
    )

    toggle value act = div [
        class_ (ClassName ("config-toggle " <> if value then "on" else "off")),
        onClick \e -> Just (Query act unit)
    ] [mtext if value then on else off]

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
            onClick \e -> send (SetMode mode)
        ] [img [src (tool mode)]]
        slot Nothing = div [slotClass false] []




styleStr :: forall i r. String -> IProp (style :: I | r) i
styleStr value = unsafeCoerce (prop (PropName "style") Nothing value)


onWheel :: forall r i. (WheelEvent -> Maybe i) -> IProp (onMouseDown :: I | r) i
onWheel = handler (EventType "wheel") <<< unsafeCoerce

mouseHandler :: forall i. (MouseEvent -> Maybe i) -> Event -> Maybe i
mouseHandler = unsafeCoerce

