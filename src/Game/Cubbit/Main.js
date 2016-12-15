"use strict";

/* global document */

exports.hideLoading = function(){
    document.getElementById("loading").style["opacity"] = "0";
};

exports.flipFaces = function(vertexData){
    return vertexData.meshes.map(function(mesh){
        var indices = mesh.indices.slice();
        for(var i = 0; i < indices.length / 3; i++){
            var temp = indices[3 * i];
            indices[3 * i] = indices[3 * i + 2];
            indices[3 * i + 2] = temp;
        }
        //var obj = Object.create({});
        //Object.assign(obj, mesh);
        //obj.indices = indices;
        //return obj;
        return mesh;
    });
};

exports.loadImageEff = function(url){
    return function(reject){
        return function(resolve){
            return function(){
                var image = new Image();
                image.onload = function(){
                    resolve(image)();
                };
                image.onerror = function(e){
                    reject(new Error("loadImage: " + url))();
                }
                image.src = url;
            }
        }
    }
}