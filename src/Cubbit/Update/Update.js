"use strict";

/* global document */

exports.foreachBlocks = function(size){
    return function(cx){
        return function(cy){
            return function(cz){
                return function(currentIndex){
                    return function(f){
                        return function(){
                            var cost = 0;
                            var limit = 100;

                            //console.log(JSON.stringify(index));

                            var index = currentIndex;
                            if( ! index){
                                index = { size: size, cx: cx, cy: cy, cz: cz, i: 0, n: 0, m: 0 };
                            }else if(index.size !== size || index.cx !== cx || index.cy !== cy || index.cz !== cz){
                                index.size = size;
                                index.cx = cx;
                                index.cy = cy;
                                index.cz = cz;
                                index.i = 0;
                                index.n = -index.i;
                                index.m = -index.i;
                            }

                            if(size <= index.i){
                                index.i = 0;
                                index.n = -index.i;
                                index.m = -index.i;
                                return index;
                            }else if(index.i < index.n){
                                index.i += 1;
                                index.n = -index.i;
                                index.m = -index.i;
                                return index;
                            }else if(index.i < index.m){
                                index.n += 1;
                                index.m = -index.i;
                                return index;
                            }else{
                                var i = index.i;
                                var n = index.n;
                                var m = index.m;

                                cost += f(cx + i)(cy + n)(cz +  m)() + 1;
                                if(limit < cost ) return index;
                                cost += f(cx - i)(cy + n)(cz + m)() + 1;
                                if(limit < cost ) return index;
                                cost += f(cx + n)(cy + i)(cz + m)() + 1;
                                if(limit < cost ) return index;
                                cost += f(cx + n)(cy - i)(cz + m)() + 1;
                                if(limit < cost ) return index;
                                cost += f(cx + n)(cy + m)(cz + i)() + 1;
                                if(limit < cost ) return index;
                                cost += f(cx + n)(cy + m)(cz - i)() + 1;
                                if(limit < cost ) return index;

                                index.m += 1;
                                return index;
                            }



                        };
                    };
                };
            };
        };
    };
};


exports.setTextContent = function(id){
    return function(text){
        return function(){
            document.getElementById(id).textContent = text;
        };
    };
};


