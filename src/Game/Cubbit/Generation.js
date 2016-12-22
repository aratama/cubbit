"use strict";

exports._createBlockMapJS = function(imports) {
    return function(noise) {
        return function(index) {

            var chunkSize = imports.chunkSize;
            var terrainScale = imports.terrainScale;
            var waterBlockHeight = imports.waterBlockHeight;
            var maxHeight = imports.maxHeight;
            var simplex2 = imports.simplex2;
            var blockTypes = imports.blockTypes;
            var runChunkIndex = imports.runChunkIndex;

            var airBlock = blockTypes.airBlock;
            var grassBlock = blockTypes.grassBlock;
            var waterBlock = blockTypes.waterBlock;
            var woodBlock = blockTypes.woodBlock;
            var leavesBlock = blockTypes.leavesBlock;
            var dirtBlock = blockTypes.dirtBlock;
            var bushBlock = blockTypes.bushBlock;

            var rci = runChunkIndex(index);
            var cx = rci.x
            var cy = rci.y
            var cz = rci.z

            var chunkBlockCount = chunkSize * chunkSize * chunkSize;

            if (cy < 0) {
                return Uint8Array.from({
                    length: chunkBlockCount
                }, function(v, k) {
                    return dirtBlock
                })
            } else if (0 < cy) {
                return new Uint8Array(chunkBlockCount)
            } else {

                // var stmap = Object.create({});
                var stmap = new Uint8Array(chunkBlockCount);

                function lookup(lx, ly, lz) {
                    if (0 <= lx && lx < chunkSize && 0 <= ly && ly < chunkSize && 0 <= lz && lz < chunkSize) {
                        return stmap[chunkSize * chunkSize * lx + chunkSize * ly + lz];
                    } else {
                        return airBlock
                    }
                }

                function put(lx, ly, lz, value) {
                    if (0 <= lx && lx < chunkSize && 0 <= ly && ly < chunkSize && 0 <= lz && lz < chunkSize) {
                        stmap[chunkSize * chunkSize * lx + chunkSize * ly + lz] = value;
                    }
                }


                function getHeight(gx, gz) {
                    var r = (simplex2(gx * terrainScale)(gz * terrainScale)(noise) + 1.0) * 0.5;
                    return Math.floor(r * maxHeight);
                }

/*
                if(cx == 0 && cy == 0 && cz == 0){
                    for (var lz = 0; lz <= chunkSize - 1; lz++) {
                        for (var lx = 0; lx <= chunkSize - 1; lx++) {
                            var h = lx < 4 ? 4 : 5;
                            for (var ly = 0; ly < h; ly++) {
                                put(lx, ly, lz, grassBlock);
                            }
                        }
                    }
                    return stmap;
                }
*/




                // terrain
                for (var lz = 0; lz <= chunkSize - 1; lz++) {
                    for (var lx = 0; lx <= chunkSize - 1; lx++) {
                        var gx = chunkSize * cx + lx
                        var gz = chunkSize * cz + lz
                        var h = getHeight(gx, gz)
                        for (var ly = 0; ly < chunkSize; ly++) {
                            var gy = chunkSize * cy + ly;
                            var ly = gy - chunkSize * cy;
                            if (gy == h) {
                                put(lx, ly, lz, grassBlock);
                            } else if (gy < h) {
                                put(lx, ly, lz, dirtBlock);
                            } else if (gy <= waterBlockHeight) {
                                put(lx, ly, lz, waterBlock);
                            }
                        }
                    }
                }

                // woods and bushes
                for (var lz = 0; lz <= chunkSize - 1; lz++) {
                    for (var lx = 0; lx <= chunkSize - 1; lx++) {
                        var gx = chunkSize * cx + lx
                        var gz = chunkSize * cz + lz
                        var h = getHeight(gx, gz)
                        if (waterBlockHeight <= h) {

                            var lh = h - chunkSize * cy


                            var r = ((simplex2(1234 + gx)(5678 + gz)(noise) + 1) * 10000) | 0;

                            if ((r % 97) === 29) {
                                // get height
                                var top = null;
                                for (var ly = chunkSize - 1; 0 <= ly; ly--) {
                                    if (lookup(lx, ly, lz) === grassBlock) {
                                        top = ly;
                                        break;
                                    }
                                }
                                if (top !== null) {
                                    for (var i = 0; i < 3 && top + 1 + i < chunkSize; i++) {
                                        put(lx, top + 1 + i, lz, woodBlock);
                                    }
                                    for (var i = 3; i < 6 && top + 1 + i < chunkSize; i++) {
                                        for (var dx = -1; dx <= 1; dx++) {
                                            for (var dz = -1; dz <= 1; dz++) {
                                                put(lx + dx, top + 1 + i, lz + dz, leavesBlock);
                                            }
                                        }
                                    }
                                }
                            } else if ((r % 93) < 10) {
                                put(lx, lh + 1, lz, bushBlock);
                            }
                        }
                    }
                }



                return stmap;
            };
        };
    };
};