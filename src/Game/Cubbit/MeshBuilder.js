"use strict";

exports.createTerrainGeometryJS = function(references) {
    return function(terrain) {
        return function(chunk) {

            var chunkSize = references.chunkSize;
            var blockTypes = references.blockTypes;
            var runChunkIndex = references.runChunkIndex;
            var blockIndex = references.blockIndex;
            var globalIndexToChunkIndex = references.globalIndexToChunkIndex;
            var globalIndexToLocalIndex = references.globalIndexToLocalIndex;
            var simplex2 = references.simplex2;

            var noise = terrain.noise;

            var chunkMap = terrain.map.map;

            var TEXTURE_SIZE = 4096;
            var CHIP_SIZE = 64;
            var CHIP_RATIO_0 = 0;
            var CHIP_RATIO_1 = CHIP_SIZE / TEXTURE_SIZE;
            var CHIP_RATIO_2 = CHIP_RATIO_1 * 2;
            var CHIP_RATIO_3 = CHIP_RATIO_1 * 3;
            var CHIP_RATIO_4 = CHIP_RATIO_1 * 4;

            function nxUV(uvs, dx) {
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
            }

            function pxUV(uvs, dx) {
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
            }

            function nyUV(uvs, dx) {
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(CHIP_RATIO_3);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(CHIP_RATIO_3);
            }

            function pyUV(uvs, dx) {
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
            }

            function nzUV(uvs, dx) {
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
            }

            function pzUV(uvs, dx) {
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
                uvs.push(CHIP_RATIO_0 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_1);
                uvs.push(CHIP_RATIO_1 + dx);
                uvs.push(1.0 - CHIP_RATIO_2);
            }

            var airBlock = blockTypes.airBlock;
            var waterBlock = blockTypes.waterBlock;
            var bushBlock = blockTypes.bushBlock;

            var blocks = chunk.blocks;

            function prepareArray() {
                return {
                    offset: 0,
                    indices: [],
                    positions: [],
                    normals: [],
                    uvs: [],
                    colors: []
                };
            }

            var standardMaterialBlockStore = prepareArray();
            var waterBlockStore = prepareArray();
            var transparentMaterialVertexData = prepareArray();

            var chunkIndex = runChunkIndex(chunk.index);
            var ox = chunkSize * chunkIndex.x;
            var oy = chunkSize * chunkIndex.y;
            var oz = chunkSize * chunkIndex.z;


            function solidBounds(block) {
                return block !== airBlock && block !== waterBlock && block !== bushBlock;
            }

            function waterBounds(block) {
                return block !== airBlock;
            }

            function exists(gx, gy, gz, bounds) {
                var lx = gx - ox;
                var ly = gy - oy;
                var lz = gz - oz;
                if (
                    0 <= lx && lx < chunkSize &&
                    0 <= ly && ly < chunkSize &&
                    0 <= lz && lz < chunkSize
                ) {
                    var t = blocks[chunkSize * chunkSize * lx + chunkSize * ly + lz];
                    return bounds(t);
                } else {
                    var gi = blockIndex(gx)(gy)(gz);
                    //var chunkWithMesh = chunkMap[globalIndexToChunkIndex(gi)];
                    var chunkIndex = globalIndexToChunkIndex(gi);
                    var chunkWithMesh = chunkMap.get(chunkIndex);
                    if (chunkWithMesh) {
                        var block = chunkWithMesh.blocks[globalIndexToLocalIndex(gi)];
                        if (typeof block == "undefined") {
                            // nerver come here
                            debugger;
                        }
                        return bounds(block);
                    } else {
                        // nerver come here?
                        var i = runChunkIndex(chunkIndex);
                        debugger;
                        return true;
                    }
                }
            }




            for (var lx = 0; lx < chunkSize; lx++) {
                for (var lz = 0; lz < chunkSize; lz++) {

                    var gx = chunkSize * chunkIndex.x + lx;
                    var gz = chunkSize * chunkIndex.z + lz;
                    var random = (simplex2(gx)(gz)(noise) + 1.0) * 0.5;

                    for (var ly = 0; ly < chunkSize; ly++) {

                        var block = blocks[chunkSize * chunkSize * lx + chunkSize * ly + lz];

                        // global coordinates of the block
                        var px = ox + lx;
                        var py = oy + ly;
                        var pz = oz + lz;

                        var store = block === waterBlock ? waterBlockStore :
                            block === bushBlock ? transparentMaterialVertexData :
                            standardMaterialBlockStore;

                        // nx, ny, nz: normal vector
                        function square(nx, ny, nz, u, bounds) {
                            if (!exists(px + nx, py + ny, pz + nz, bounds)) {

                                // horizontal extent vector of the plane
                                var ax = ny;
                                var ay = nz;
                                var az = nx;

                                // vertical extent vector of the plane
                                var bx = ay * nz - ay * nx;
                                var by = az * nx - ax * nz;
                                var bz = ax * ny - ay * nx;

                                // half-sized normal vector
                                var dx = nx * 0.5;
                                var dy = ny * 0.5;
                                var dz = nz * 0.5;

                                // half-sized horizontal vector
                                var sx = ax * 0.5;
                                var sy = ay * 0.5;
                                var sz = az * 0.5;

                                // half-sized vertical vector
                                var tx = bx * 0.5;
                                var ty = by * 0.5;
                                var tz = bz * 0.5;

                                // center of the plane
                                var vx = px + 0.5 + dx;
                                var vy = py + 0.5 + dy;
                                var vz = pz + 0.5 + dz;

                                // vertex index offset
                                var offset = store.offset;

                                store.indices.push(offset + 0);
                                store.indices.push(offset + 1);
                                store.indices.push(offset + 2);
                                store.indices.push(offset + 0);
                                store.indices.push(offset + 2);
                                store.indices.push(offset + 3);

                                store.positions.push(vx - sx - tx);
                                store.positions.push(vy - sy - ty);
                                store.positions.push(vz - sz - tz);
                                store.positions.push(vx + sx - tx);
                                store.positions.push(vy + sy - ty);
                                store.positions.push(vz + sz - tz);
                                store.positions.push(vx + sx + tx);
                                store.positions.push(vy + sy + ty);
                                store.positions.push(vz + sz + tz);
                                store.positions.push(vx - sx + tx);
                                store.positions.push(vy - sy + ty);
                                store.positions.push(vz - sz + tz);

                                store.normals.push(nx);
                                store.normals.push(ny);
                                store.normals.push(nz);
                                store.normals.push(nx);
                                store.normals.push(ny);
                                store.normals.push(nz);
                                store.normals.push(nx);
                                store.normals.push(ny);
                                store.normals.push(nz);
                                store.normals.push(nx);
                                store.normals.push(ny);
                                store.normals.push(nz);


                                var add = 0.2;
                                var base = 0.4;

                                var brightness =
                                    (exists(px + nx - ax, py + ny - ay, pz + nz - az, bounds) ? 0 : add) +
                                    (exists(px + nx - bx, py + ny - by, pz + nz - bz, bounds) ? 0 : add) +
                                    (exists(px + nx - ax - bx, py + ny - ay - by, pz + nz - az - bz, bounds) ? 0 : add) + base;

                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(1.0);


                                var brightness =
                                    (exists(px + nx + ax, py + ny + ay, pz + nz + az, bounds) ? 0 : add) +
                                    (exists(px + nx - bx, py + ny - by, pz + nz - bz, bounds) ? 0 : add) +
                                    (exists(px + nx + ax - bx, py + ny + ay - by, pz + nz + az - bz, bounds) ? 0 : add) + base;
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(1.0);

                                var brightness =
                                    (exists(px + nx + ax, py + ny + ay, pz + nz + az, bounds) ? 0 : add) +
                                    (exists(px + nx + bx, py + ny + by, pz + nz + bz, bounds) ? 0 : add) +
                                    (exists(px + nx + ax + bx, py + ny + ay + by, pz + nz + az + bz, bounds) ? 0 : add) + base;
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(1.0);

                                var brightness =
                                    (exists(px + nx - ax, py + ny - ay, pz + nz - az, bounds) ? 0 : add) +
                                    (exists(px + nx + bx, py + ny + by, pz + nz + bz, bounds) ? 0 : add) +
                                    (exists(px + nx - ax + bx, py + ny - ay + by, pz + nz - az + bz, bounds) ? 0 : add) + base;
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(brightness);
                                store.colors.push(1.0);

                                //u(store.uvs, CHIP_RATIO_1 * block);
                                u(store.uvs, CHIP_RATIO_1 * block);

                                store.offset += 4;
                            }
                        }

                        function bush() {
                            var bushHeight = 1.0;

                            var offset = store.offset;

                            store.indices.push(offset + 0);
                            store.indices.push(offset + 1);
                            store.indices.push(offset + 2);
                            store.indices.push(offset + 0);
                            store.indices.push(offset + 2);
                            store.indices.push(offset + 3);

                            store.indices.push(offset + 2);
                            store.indices.push(offset + 1);
                            store.indices.push(offset + 0);
                            store.indices.push(offset + 3);
                            store.indices.push(offset + 2);
                            store.indices.push(offset + 0);

                            store.indices.push(offset + 4 + 0);
                            store.indices.push(offset + 4 + 1);
                            store.indices.push(offset + 4 + 2);
                            store.indices.push(offset + 4 + 0);
                            store.indices.push(offset + 4 + 2);
                            store.indices.push(offset + 4 + 3);

                            store.indices.push(offset + 4 + 2);
                            store.indices.push(offset + 4 + 1);
                            store.indices.push(offset + 4 + 0);
                            store.indices.push(offset + 4 + 3);
                            store.indices.push(offset + 4 + 2);
                            store.indices.push(offset + 4 + 0);


                            var rot = Math.PI * 2 * random;
                            //var rot = Math.PI * 0.20;
                            var rec = Math.PI * 0.5;
                            var w = 0.6;

                            var cx = px + 0.5;
                            var cz = pz + 0.5;

                            var x0 = cx + Math.cos(rot + rec * 0) * w;
                            var z0 = cz + Math.sin(rot + rec * 0) * w;

                            var x1 = cx + Math.cos(-rot + rec * 1) * w;
                            var z1 = cz + Math.sin(-rot + rec * 1) * w;

                            var x2 = cx + Math.cos(rot + rec * 2) * w;
                            var z2 = cz + Math.sin(rot + rec * 2) * w;

                            var x3 = cx + Math.cos(-rot + rec * 3) * w;
                            var z3 = cz + Math.sin(-rot + rec * 3) * w;

                            store.positions.push(x0);
                            store.positions.push(py);
                            store.positions.push(z0);

                            store.positions.push(x2);
                            store.positions.push(py);
                            store.positions.push(z2);

                            store.positions.push(x2);
                            store.positions.push(py + bushHeight);
                            store.positions.push(z2);

                            store.positions.push(x0);
                            store.positions.push(py + bushHeight);
                            store.positions.push(z0);

                            store.positions.push(x3);
                            store.positions.push(py);
                            store.positions.push(z1);

                            store.positions.push(x1);
                            store.positions.push(py);
                            store.positions.push(z3);

                            store.positions.push(x1);
                            store.positions.push(py + bushHeight);
                            store.positions.push(z3);

                            store.positions.push(x3);
                            store.positions.push(py + bushHeight);
                            store.positions.push(z1);

                            var nx = 0,
                                ny = 0,
                                nz = 1;
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);

                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);
                            store.normals.push(nx);
                            store.normals.push(ny);
                            store.normals.push(nz);

                            var r = 1,
                                g = 1,
                                b = 1;

                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);

                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);
                            store.colors.push(r);
                            store.colors.push(g);
                            store.colors.push(b);
                            store.colors.push(1.0);


                            store.uvs.push(0);
                            store.uvs.push(500 / 4096);
                            store.uvs.push(1000 / 4096);
                            store.uvs.push(500 / 4096);
                            store.uvs.push(1000 / 4096);
                            store.uvs.push(0);
                            store.uvs.push(0);
                            store.uvs.push(0);


                            store.uvs.push(0);
                            store.uvs.push(500 / 4096);
                            store.uvs.push(1000 / 4096);
                            store.uvs.push(500 / 4096);
                            store.uvs.push(1000 / 4096);
                            store.uvs.push(0);
                            store.uvs.push(0);
                            store.uvs.push(0);

                            store.offset += 8;
                        }

                        switch (block) {
                            case airBlock:
                                break;
                            case waterBlock:
                                square(-1, 0, 0, nxUV, waterBounds);
                                square(1, 0, 0, pxUV, waterBounds);
                                square(0, -1, 0, nyUV, waterBounds);
                                square(0, 1, 0, pyUV, waterBounds);
                                square(0, 0, -1, nzUV, waterBounds);
                                square(0, 0, 1, pzUV, waterBounds);
                                break;

                            case bushBlock:
                                bush();
                                break;

                            default:
                                square(-1, 0, 0, nxUV, solidBounds);
                                square(1, 0, 0, pxUV, solidBounds);
                                square(0, -1, 0, nyUV, solidBounds);
                                square(0, 1, 0, pyUV, solidBounds);
                                square(0, 0, -1, nzUV, solidBounds);
                                square(0, 0, 1, pzUV, solidBounds);
                                break;
                        }
                    }
                }
            }

            return {
                standardMaterialBlocks: standardMaterialBlockStore,
                waterMaterialBlocks: waterBlockStore,
                transparentMaterialVertexData: transparentMaterialVertexData
            };
        };
    };
};