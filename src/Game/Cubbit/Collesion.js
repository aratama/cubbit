"use strict";

exports._buildCollesionBoxes = function(imports){
    return function(chunk) {
        return function(world) {
            return function() {

                var localIndex = imports.localIndex;
                var chunkSize = imports.chunkSize;

                var blocks = chunk.blocks; // Uint8Array

                var sx = 1.0;
                var sy = 1.0;
                var sz = 1.0;



                var map = [];       // Array Boolean
                var boxes = [];     // Array (Body String)
                var boxified = [];  // Array Boolean
                var boxShape = new CANNON.Box(new CANNON.Vec3(sx * 0.5, sy * 0.5, sz * 0.5));




                // Prepare map
                for (var i = 0; i !== chunkSize; i++) {
                    for (var j = 0; j !== chunkSize; j++) {
                        for (var k = 0; k !== chunkSize; k++) {
                            map.push(true);
                            boxified.push(false);
                        }
                    }
                }




                //// copy data
                for (var i = 0; i !== chunkSize; i++) {
                    for (var j = 0; j !== chunkSize; j++) {
                        for (var k = 0; k !== chunkSize; k++) {
                            var index = getBoxIndex(i, j, k);
                            map[index] = blocks[index] !== 0;
                        }
                    }
                }




                // User must manually update the map for the first time.
                function contains(xi, yi, zi){
                    return  xi >= 0 && xi < chunkSize &&
                            yi >= 0 && yi < chunkSize &&
                            zi >= 0 && zi < chunkSize;
                }

                function getBoxIndex(xi, yi, zi) {
                    return localIndex(xi)(yi)(zi);
                }

                function setFilled(xi, yi, zi, filled) {
                    if(contains(xi, yi, zi)){
                        map[getBoxIndex(xi, yi, zi)] = !!filled
                    }
                }

                function isFilled(xi, yi, zi) {
                    return contains(xi, yi, zi) && map[getBoxIndex(xi, yi, zi)];
                }

                function isBoxified(xi, yi, zi) {
                    return contains(xi, yi, zi) && boxified[getBoxIndex(xi, yi, zi)];
                }

                function setBoxified(xi, yi, zi, v) {
                    boxified[getBoxIndex(xi, yi, zi)] = !!v;
                }

                while (true) {
                    var box;

                    // 1. Get a filled box that we haven't boxified yet
                    for (var i = 0; !box && i < chunkSize; i++) {
                        for (var j = 0; !box && j < chunkSize; j++) {
                            for (var k = 0; !box && k < chunkSize; k++) {
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
                                }
                            }
                        }
                    }

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
                        for (var i = xi; i < chunkSize + 1; i++) {
                            if ( ! isFilled(i, yi, zi) || (isBoxified(i, yi, zi) && contains(i, yi, zi))) {
                                // Can't merge this box. Make sure we limit the mergeing
                                box.nx = i - xi;
                                break;
                            }
                        }

                        // Merge in Z
                        var found = false;
                        for (var i = xi; !found && i < xi + box.nx; i++) {
                            for (var j = zi; !found && j < chunkSize + 1; j++) {
                                if ( ! isFilled(i, yi, j) || (isBoxified(i,yi, j) && contains(i, yi, j))) {
                                    // Can't merge this box. Make sure we limit the mergeing
                                    if (box.nz > j - zi) box.nz = j - zi;
                                }
                            }
                        }

                        // Merge in Y
                        found = false;
                        for (var i = xi; !found && i < xi + box.nx; i++) {
                            for (var j = zi; !found && j < zi + box.nz; j++) {
                                for (var k = yi; k < chunkSize + 1; k++) {
                                    if ( ! isFilled(i, k, j) || (isBoxified(i, k, j) && contains(i, k, j))) {
                                        // Can't merge this box. Make sure we limit the mergeing
                                        if (box.ny > k - yi) box.ny = k - yi;
                                    }
                                }
                            }
                        }

                        if (box.nx == 0) box.nx = 1;
                        if (box.ny == 0) box.ny = 1;
                        if (box.nz == 0) box.nz = 1;

                        // Set the merged boxes as boxified
                        for (var i = xi; i < xi + box.nx; i++) {
                            for (var j = yi; j < yi + box.ny; j++) {
                                for (var k = zi; k < zi + box.nz; k++) {
                                    if (i >= xi && i <= xi + box.nx &&
                                        j >= yi && j <= yi + box.ny &&
                                        k >= zi && k <= zi + box.nz) {
                                        setBoxified(i, j, k, true);
                                    }
                                }
                            }
                        }

                        box = false;
                    } else {
                        break;
                    }
                }

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

                return boxes;
            }
        }
    }
}