exports.foreachBlocks = function(size){
    return function(cx){
        return function(cy){
            return function(cz){
                return function(index){
                    return function(f){
                        return function(){
                            var cost = 0;
                            var limit = 100;

                            if( ! index){
                                index = { size: size, cx: cx, cy: cy, cz: cz, i: 0, n: -size, m: -size };
                            }else if(index.size != size || index.cx != cx || index.cy != cy || index.cz != cz){
                                index.size = size;
                                index.cx = cx;
                                index.cy = cy;
                                index.cz = cz;
                                index.i = 0;
                                index.n = -size;
                                index.m = -size;
                            }

                            if(size <= index.i){
                                index.i = 0;
                                index.n = -size;
                                index.m = -size;
                                return index;
                            }else if(size < index.n){
                                index.i += 1;
                                index.n = -size;
                                index.m = -size;
                                return index;
                            }else if(size < index.m){
                                index.n += 1;
                                index.m = -size;
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



                        }
                    }
                }
            }
        }
    }
}