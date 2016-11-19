exports.createBlockMapJS = function(noise){
    return function(simplex2){
        return function(index){
            return function(terrainScale){
                return function(waterBlockHeight){
                    return function(maxHeight){
                        return function(blockTypes){

                            var airBlock = blockTypes.airBlock;
                            var grassBlock = blockTypes.grassBlock;
                            var waterBlock = blockTypes.waterBlock;
                            var woodBlock = blockTypes.woodBlock;
                            var leavesBlock = blockTypes.leavesBlock;

                            var runBlockIndex = PS["Game.Cubbit.BlockIndex"].runBlockIndex;
                            var runChunkIndex = PS["Game.Cubbit.ChunkIndex"].runChunkIndex;
                            var blockIndex = PS["Game.Cubbit.BlockIndex"].blockIndex;
                            var chunkSize = PS["Game.Cubbit.Constants"].chunkSize;

                            var rci = runChunkIndex(index);
                            var cx = rci.x
                            var cy = rci.y
                            var cz = rci.z

                            // var stmap = Object.create({});
                            var stmap = new Uint8Array(chunkSize * chunkSize * chunkSize);

                            function lookup(lx, ly, lz){
                                if(0 <= lx && lx < chunkSize && 0 <= ly && ly < chunkSize && 0 <= lz && lz < chunkSize){
                                    return stmap[chunkSize * chunkSize * lx + chunkSize * ly + lz];
                                }else{
                                    return airBlock
                                }
                            }
                            function put(lx, ly, lz, value){
                                if(0 <= lx && lx < chunkSize && 0 <= ly && ly < chunkSize && 0 <= lz && lz < chunkSize){
                                    stmap[chunkSize * chunkSize * lx + chunkSize * ly + lz] = value;
                                }
                            }


                            // terrain
                            for(var lz = 0; lz <= chunkSize - 1; lz++){
                                for(var lx = 0; lx <= chunkSize - 1; lx++){
                                    var gx = chunkSize * cx + lx
                                    var gz = chunkSize * cz + lz
                                    var x = gx
                                    var z = gz
                                    var r = (simplex2(x * terrainScale)(z * terrainScale)(noise) + 1.0) * 0.5
                                    var h = Math.max(waterBlockHeight, Math.floor(r * maxHeight))
                                    var top = Math.min(h, chunkSize * (cy + 1) - 1)
                                    var bottom = chunkSize * cy
                                    if(top < bottom){

                                    }else{
                                        for(var gy = bottom; gy <= top; gy++){
                                            var ly = gy - chunkSize * cy;
                                            var blockType = gy <= waterBlockHeight ? waterBlock : grassBlock;
                                            put(lx, ly, lz, blockType);
                                        }
                                    }
                                }
                            }

                            // woods
                            for(var lz = 0; lz <= chunkSize - 1; lz++){
                                for(var lx = 0; lx <= chunkSize - 1; lx++){
                                    var gx = chunkSize * cx + lx
                                    var gz = chunkSize * cz + lz
                                    var r = Math.random();
                                    if(0.99 < r){
                                        // get height
                                        var top = null;
                                        for(var ly = chunkSize - 1; 0 <= ly; ly--){
                                            if(lookup(lx, ly, lz) === grassBlock){
                                                top = ly;
                                                break;
                                            }
                                        }
                                        if(top !== null){
                                            for(var i = 0; i < 3 && top + 1 + i < chunkSize; i++){
                                                put(lx, top + 1 + i, lz, woodBlock);
                                            }
                                            for(var i = 3; i < 6 && top + 1 + i < chunkSize; i++){
                                                for(var dx = -1; dx <= 1; dx++){
                                                    for(var dz = -1; dz <= 1; dz++){
                                                        put(lx + dx, top + 1 + i, lz + dz, leavesBlock);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }


                            return {
                                index: index,
                                blocks: stmap
                            }
                        }
                    }
                }
            }
        }
    }
}