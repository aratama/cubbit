exports.compress = function(string){
    return LZString.compress(string);
};

exports._decompress = function(string){
    return LZString.decompress(string);
};

exports.compressToUTF16 = function(string){
    return LZString.compressToUTF16(string);
};

exports._decompressFromUTF16 = function(string){
    return LZString.decompressFromUTF16(string);
};
