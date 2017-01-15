module Game.Cubbit.Physics (createPlayerCollesion) where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Cannon (addShape, createBody, createMaterial, createSphere, createVec3, defaultBodyProps, setFixedRotation, setPosition, updateMassProperties)
import Graphics.Cannon.Type (Body, CANNON)
import Prelude (bind, pure)

createPlayerCollesion :: forall eff. Eff (cannon :: CANNON | eff) Body
createPlayerCollesion = do
    size <- createVec3 0.5 0.5 0.5
    pos <- createVec3 2.5 17.0 2.5
    mat <- createMaterial {
        friction: 0.0000,
        restitution: 0.0
    }

    body <- createBody defaultBodyProps {
        mass = 1.0,
        material = mat
    }

    upper <- createSphere 0.4
    upperOffset <- createVec3 0.0 (1.2) 0.0
    addShape upper (Just upperOffset) Nothing body
    lower <- createSphere 0.4
    lowerOffset <- createVec3 0.0 (0.4) 0.0
    addShape lower (Just lowerOffset) Nothing body

    setPosition pos body
    setFixedRotation true body
    updateMassProperties body
    pure body


