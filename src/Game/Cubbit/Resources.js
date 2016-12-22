"use strict";

/* global document */


exports.loadScript = function(url){
    return function(reject){
        return function(resolve){
            return function(){
                var script = document.createElement("script");
                script.onload = function(){
                    document.head.appendChild(script);
                    resolve(script)();
                };
                script.onerror = function(e){
                    reject(e)();
                };
                script.setAttribute("src", url);
            }
        }
    }
}