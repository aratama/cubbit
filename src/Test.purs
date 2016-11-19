module Graphics.Babylon.Test where

import Control.Bind (bind)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Example.Sandbox.BlockIndex (blockIndex, runBlockIndex)
import Graphics.Babylon.Example.Sandbox.ChunkIndex (chunkIndex)
import Graphics.Babylon.Example.Sandbox.Terrain (globalPositionToChunkIndex, globalPositionToLocalIndex, globalPositionToGlobalIndex)
import Prelude (mod, negate, ($), (&&), (<), (==))
import Test.StrongCheck (SC, assert, assertEq, quickCheck)

main :: SC (babylon :: BABYLON) Unit
main = do

    assert $ assertEq (globalPositionToChunkIndex 0.5 0.5 0.5) (chunkIndex 0 0 0)
    assert $ assertEq (globalPositionToChunkIndex (-0.5) (-0.5) (-0.5)) (chunkIndex (-1) (-1) (-1))
    assert $ assertEq (globalPositionToChunkIndex (35.0) (-0.5) (12.5)) (chunkIndex (2) (-1) (0))
    assert $ assertEq (globalPositionToChunkIndex (-35.0) (0.5) (-12.5)) (chunkIndex (-3) (0) (-1))

    assert $ assertEq (globalPositionToLocalIndex (-9.3) (6.5) (-4.9)) (blockIndex (6) (6) (11))
    assert $ assertEq (globalPositionToLocalIndex (15.9) (16.0) (16.1)) (blockIndex (15) (0) (0))
    assert $ assertEq (globalPositionToLocalIndex (-15.9) (-16.0) (-16.1)) (blockIndex (0) (0) (15))
    assert $ assertEq (globalPositionToLocalIndex (-31.9) (-32.0) (-32.1)) (blockIndex (0) (0) (15))

    assert $ assertEq (globalPositionToGlobalIndex (0.0) (1.0) (-1.0)) (blockIndex (0) 1 (-1))
    assert $ assertEq (globalPositionToGlobalIndex (-0.9) (-1.0) (-1.1)) (blockIndex (-1) (-1) (-2))
    assert $ assertEq (globalPositionToGlobalIndex (-15.9) (-16.0) (-16.1)) (blockIndex (-16) (-16) (-17))


    assert $ assertEq let xyz = runBlockIndex (blockIndex 1 2 3) in xyz.x == 1 && xyz.y == 2 && xyz.z == 3
    assert $ assertEq let xyz = runBlockIndex (blockIndex (-1) (-2) (-3)) in xyz.x == (-1) && xyz.y == (-2) && xyz.z == (-3)

    let checkRange n = (-65536) < n && n < 65536
    quickCheck \x y z -> let
        nx = mod x 1000
        ny = mod y 1000
        nz = mod z 1000
        xyz = runBlockIndex (blockIndex nx ny nz)
        in
        nx == xyz.x && ny == xyz.y && nz == xyz.z
