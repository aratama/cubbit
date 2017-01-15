"use strict";

/* global Uint8Array */

exports._empty = function(length){
    return new Uint8Array(length);
}

exports.insert = function(key){
    return function(value){
        return function(map){
            var m = new Uint8Array(map);
            m[key] = value;
            return m;
        };
    };
};

exports.delete = function(key){
    return function(map){
        var m = new Uint8Array(map);
        m[key] = 0;
        return m;
    };
};

exports.lookupNullable = function(key){
    return function(map){
        return map[key];
    };
};