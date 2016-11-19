exports.showBlockIndex = function(i){
    return "" + i;
}


/*
exports.blockIndex = function(x){
    return function(y){
        return function(z){
            return x + "," + y + "," + z;
        }
    }
}

exports.runBlockIndex = function(bits){
    var ns = bits.split(",");
    return {
        x: parseInt(ns[0]),
        y: parseInt(ns[1]),
        z: parseInt(ns[2])
    }
}
*/

var _2_13 = 8192;      // 2^13
var _2_14 = 16384;      // 2^14
var _2_17 = 131072;     // 2^17
var _2_32 = 4294967296; // 2^32

exports.blockIndex = function(x){
    return function(y){
        return function(z){
            return (x + _2_17) * _2_32 + (y + _2_17) * _2_14 + (z + _2_13)
        }
    }
}

exports.runBlockIndex = function(bits){
    var cx = (bits / _2_32) | 0;
    var cy = ((bits - cx * _2_32) / _2_14) | 0;
    var cz = (bits - cx * _2_32 - cy * _2_14);
    var x = cx - _2_17;
    var y = cy - _2_17;
    var z = cz - _2_13;
    return { x: x, y: y, z: z }
}
