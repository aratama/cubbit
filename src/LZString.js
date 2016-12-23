exports.compress = function(string){
    return LZString.compress(string);
};

exports.decompress = function(string){
    return LZString.decompress(string);
};
