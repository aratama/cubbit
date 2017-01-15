module Game.Cubbit.Materials (Materials, initializeMaterials) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Game.Cubbit.Option (Options(Options))
import Graphics.Babylon.BaseTexture (setHasAlpha)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.Material (setAlpha)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.ShaderMaterial (createShaderMaterial, setColor3, setFloats, setTexture, setVector3, shaderMaterialToMaterial)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setDiffuseColor, setDiffuseTexture, setSpecularColor, setUseAlphaFromDiffuseTexture, standardMaterialToMaterial)
import Graphics.Babylon.Texture (createTexture, defaultCreateTextureOptions, textureToBaseTexture)
import Graphics.Babylon.Types (BABYLON, Material, Mesh, Scene, Texture)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.WaterMaterial (createWaterMaterial, setBumpTexture, addToRenderList, waterMaterialToMaterial, setWaveHeight, setWindForce, setColorBlendFactor, setBackFaceCulling)
import Prelude (negate, (/))

type Materials = {
    blockMaterial :: Material,
    waterMaterial :: Material,
    cellShadingMaterial :: Material,
    akaneCellShadingMaterial :: Material,
    bushMaterial :: Material,
    outlineMaterial :: Material
}

initializeMaterials :: forall eff. Scene -> Mesh -> Texture -> Texture -> Options -> Eff (babylon :: BABYLON | eff) Materials
initializeMaterials scene skybox texture alphaTexture (Options options) = do

    setHasAlpha true (textureToBaseTexture alphaTexture)

    let createCellShadingMaterial textureURL = do

            mat <- createShaderMaterial "cellShading" scene "./alice/cellShading" {
                needAlphaBlending: false,
                needAlphaTesting: false,
                attributes: ["position", "uv", "normal", "matricesIndices", "matricesWeights"],
                uniforms: ["world", "viewProjection", "mBones"],
                samplers: ["textureSampler"],
                defines: []
            }
            lightPosition <- createVector3 0.0 20.0 (-10.0)
            lightColor <- createColor3 1.0 1.0 1.0
            cellShadingMaterialTexture <- createTexture textureURL scene defaultCreateTextureOptions
            setTexture "textureSampler" cellShadingMaterialTexture mat
            setVector3 "vLightPosition" lightPosition mat
            setFloats "ToonThresholds" [0.2, -0.45, -5.0, -5.0] mat
            setFloats "ToonBrightnessLevels" [1.0, 0.9, 0.75, 0.75, 0.75] mat
            setColor3 "vLightColor" lightColor mat
            pure mat



    cellShadingMaterial <- createCellShadingMaterial "./alice/texture.png"
    akaneCellShadingMaterial <- createCellShadingMaterial "./akane/texture.png"

    outlineMaterial <- do
        mat <- createShaderMaterial "outlineShaderMaterial" scene "./alice/outline" {
            needAlphaBlending: false,
            needAlphaTesting: false,
            attributes: ["position", "normal", "matricesIndices", "matricesWeights"],
            uniforms: ["world", "viewProjection", "mBones"],
            samplers: [],
            defines: []
        }
        pure mat


    solidBlockMaterial <- do
        mat <- createStandardMaterial "grass-block" scene
        grassSpecular <- createColor3 0.0 0.0 0.0
        setSpecularColor grassSpecular mat
        -- setSpecularPower 0.0 mat
        setDiffuseTexture texture mat
        pure mat

    waterMaterial <- if options.enableWaterMaterial
        then do
            mat <- createWaterMaterial "water-block" scene
            tex <- createTexture "./lib/waterbump.png" scene defaultCreateTextureOptions
            setBumpTexture tex mat
            addToRenderList (meshToAbstractMesh skybox) mat
            setWaveHeight 0.0 mat
            setWindForce 1.0 mat
            setBackFaceCulling true mat
            --setColorBlendFactor 0.1 mat
            pure (waterMaterialToMaterial mat)
        else do
            mat <- createStandardMaterial "water-block" scene
            color <- createColor3 (50.0 / 255.0) (50.0 / 255.0) (60.0 / 255.0)
            setDiffuseColor color mat
            -- setDiffuseTexture texture mat
            setAlpha 0.7 (standardMaterialToMaterial mat)
            pure (standardMaterialToMaterial mat)

    bushMaterial <- do
        mat <- createStandardMaterial "bush-material" scene
        setDiffuseTexture alphaTexture mat
        setAlpha 0.8 (standardMaterialToMaterial mat)
        color <- createColor3 0.35 0.5 0.30
        setDiffuseColor color mat
        setUseAlphaFromDiffuseTexture true mat
        pure mat


    pure {
        blockMaterial: standardMaterialToMaterial solidBlockMaterial,
        waterMaterial: waterMaterial,
        cellShadingMaterial: shaderMaterialToMaterial cellShadingMaterial,
        akaneCellShadingMaterial: shaderMaterialToMaterial akaneCellShadingMaterial,
        bushMaterial: standardMaterialToMaterial bushMaterial,
        outlineMaterial: shaderMaterialToMaterial outlineMaterial
    }
