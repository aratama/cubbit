"use strict";

exports._buildCollesionBoxes = function(imports){
    return function(chunk) {
        return function(world) {
            return function() {

                //console.profile("collesion");
                console.time("collesion");

                var localIndex = imports.localIndex;
                var chunkSize = imports.chunkSize;
                var isSolidBlock = imports.isSolidBlock;


                var blocks = chunk.blocks; // Uint8Array

                var sx = 1.0;
                var sy = 1.0;
                var sz = 1.0;

                var boxes = [];     // Array (Body String)
                var boxified = new Array(chunkSize * chunkSize * chunkSize);  // Array Boolean
                var boxShape = new CANNON.Box(new CANNON.Vec3(sx * 0.5, sy * 0.5, sz * 0.5));


                // User must manually update the map for the first time.
                function contains(xi, yi, zi){
                    return  xi >= 0 && xi < chunkSize &&
                            yi >= 0 && yi < chunkSize &&
                            zi >= 0 && zi < chunkSize;
                }

                function getBoxIndex(xi, yi, zi) {
                    return localIndex(xi)(yi)(zi);
                }

                function isFilled(xi, yi, zi) {
                    //console.assert(contains(xi, yi, zi));
                    //return map[getBoxIndex(xi, yi, zi)];
                    return isSolidBlock(blocks[getBoxIndex(xi, yi, zi)]);
                }

                function isBoxified(xi, yi, zi) {
                    //console.assert(contains(xi, yi, zi));
                    return boxified[getBoxIndex(xi, yi, zi)];
                }

                function setBoxified(xi, yi, zi, v) {
                    //console.assert(contains(xi, yi, zi));
                    boxified[getBoxIndex(xi, yi, zi)] = v;
                }

                //console.time("fill");
                //console.profile("fill");
                while (true) {
                    var box;

                    // 1. Get a filled box that we haven't boxified yet
                    (function(){
                        for (var i = 0; i < chunkSize; i++) {
                            for (var j = 0; j < chunkSize; j++) {
                                for (var k = 0; k < chunkSize; k++) {
                                    if (isFilled(i, j, k) && ! isBoxified(i, j, k)) {
                                        box = new CANNON.Body({
                                            mass: 0
                                        });
                                        box.xi = i; // Position
                                        box.yi = j;
                                        box.zi = k;
                                        box.nx = 0; // Size
                                        box.ny = 0;
                                        box.nz = 0;
                                        boxes.push(box);
                                        return;
                                    }
                                }
                            }
                        }
                    })();

                    // 2. Check if we can merge it with its neighbors
                    if (box) {

                        // Check what can be merged
                        var xi = box.xi,
                            yi = box.yi,
                            zi = box.zi;
                            box.nx = chunkSize, // merge=1 means merge just with the self box
                            box.ny = chunkSize,
                            box.nz = chunkSize;

                        // Merge in x
                        for (var i = xi; i < chunkSize; i++) {
                            if ( ! isFilled(i, yi, zi) || isBoxified(i, yi, zi)) {
                                // Can't merge this box. Make sure we limit the mergeing
                                break;
                            }
                        }
                        box.nx = i - xi;

                        // Merge in Z
                        for (var i = xi; i < xi + box.nx; i++) {
                            for (var j = zi; j < chunkSize; j++) {
                                if ( ! isFilled(i, yi, j) || isBoxified(i,yi, j)) {
                                    // Can't merge this box. Make sure we limit the mergeing
                                    break;
                                }
                            }
                            if (box.nz > j - zi) box.nz = j - zi;
                        }

                        // Merge in Y
                        for (var i = xi; i < xi + box.nx; i++) {
                            for (var j = zi; j < zi + box.nz; j++) {
                                for (var k = yi; k < chunkSize; k++) {
                                    if ( ! isFilled(i, k, j) || isBoxified(i, k, j)) {
                                        // Can't merge this box. Make sure we limit the mergeing
                                        break;
                                    }
                                }
                                if (box.ny > k - yi) box.ny = k - yi;
                            }
                        }

                        // Set the merged boxes as boxified
                        for (var i = xi; i < xi + box.nx; i++) {
                            for (var j = yi; j < yi + box.ny; j++) {
                                for (var k = zi; k < zi + box.nz; k++) {
                                    setBoxified(i, j, k, true);
                                }
                            }
                        }

                        box = false;
                    } else {
                        break;
                    }
                }
                //console.profileEnd("fill");
                //console.timeEnd("fill");

                //console.time("body");
                // Set box positions
                var sx = sx,
                    sy = sy,
                    sz = sz;
                for (var i = 0; i < boxes.length; i++) {
                    var b = boxes[i];
                    b.position.set(
                        chunkSize * chunk.x + b.xi * sx + b.nx * sx * 0.5,
                        chunkSize * chunk.y + b.yi * sy + b.ny * sy * 0.5,
                        chunkSize * chunk.z + b.zi * sz + b.nz * sz * 0.5
                    );

                    b.material = new CANNON.Material({
                        friction: 0.0,
                        restitution: 0.0
                    });

                    // Replace box shapes
                    b.addShape(new CANNON.Box(new CANNON.Vec3(b.nx * sx * 0.5, b.ny * sy * 0.5, b.nz * sz * 0.5)));
                    //b.aabbNeedsUpdate = true;
                    world.addBody(b);
                    //boxes.push(box);
                }
                //console.timeEnd("body");


                //console.profileEnd("collesion");
                console.timeEnd("collesion");
                console.log(boxes.length);

                return boxes;
            }
        }
    }
}