"use strict";

/* global document */

exports.requestPointerLock = function(callback){
    return function(onExit){
        return function(){
            var mousemoveListener = function(e){
                callback(e)();
            };
            var pointerlockchangeListener = function(){
                if( ! document.pointerLockElement){
                    document.body.removeEventListener("mousemove", mousemoveListener);
                    document.removeEventListener("pointerlockchange", pointerlockchangeListener);
                    onExit();
                }
            };
            document.body.addEventListener("mousemove", mousemoveListener);
            document.addEventListener("pointerlockchange", pointerlockchangeListener);

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
    };
};

exports.exitPointerLock = function(){
    document.exitPointerLock();
};