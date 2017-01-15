"use strict";

/* global document */


exports.addPointerlockchangeListener = function(callback){
    return function(){
        document.addEventListener("pointerlockchange", function(){
            callback(document.pointerLockElement)();
        });
    }
}

exports.addPointerMoveListener = function(callback){
    return function(){
        var mx = 0;
        var my = 0;
        document.body.addEventListener("mousemove", function(e){
            if(document.pointerLockElement && (mx != e.movementX || my != e.movementY)){
                mx = e.movementX;
                my = e.movementY;
                callback(e)();
            }
        });
    }
}

exports.requestPointerLock = function(){
    if(document.body.requestPointerLock){
        document.body.requestPointerLock();
    }else if(document.body.webkitRequestPointerLock){
        document.body.webkitRequestPointerLock();
    }else if(document.body.mozRequestPointerLock){
        document.body.mozRequestPointerLock();
    }else if(document.body.msRequestPointerLock){
        document.body.msRequestPointerLock();
    }
};

exports.exitPointerLock = function(){
    if(document.exitPointerLock){
        document.exitPointerLock();
    }
};