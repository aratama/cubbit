
var _2_15 = 32768;       // 2^15
var _2_16 = 65536;       // 2^16
var _2_17 = 131072;      // 2^17
var _2_34 = 17179869184; // 2^34

exports.blockIndex = function(x){
    return function(y){
        return function(z){
            return (x + _2_17) * _2_34 + (z + _2_17) * _2_16 + (y + _2_15)
        }
    }
}

exports.runBlockIndex = function(bits){
    var cx = (bits / _2_34) | 0;
    var cz = ((bits - cx * _2_34) / _2_16) | 0;
    var cy = (bits - cx * _2_34 - cz * _2_16);
    var x = cx - _2_17;
    var z = cz - _2_17;
    var y = cy - _2_15;
    return { x: x, y: y, z: z }
}

exports.eqBlockIndex = function(a){
    return function(b){
        return a === b;
    }
}

exports.showBlockIndex = function(a){
    return a.toString();
}
