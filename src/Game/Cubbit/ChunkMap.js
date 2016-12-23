"use strict";



function CMap(){
    this.array = new Array(10000);
    for(var i = 0; i < this.array.length; i++){
        this.array[i] = [];
    }
}
CMap.prototype.hash = function(index){
    return index % this.array.length;
};
CMap.prototype.get = function(index){
    var xs = this.array[this.hash(index)];
    for(var i = 0; i < xs.length; i++){
        if(xs[i].index === index){
            return xs[i];
        }
    }
};
CMap.prototype.set = function(index, value){
    var xs = this.array[this.hash(index)];
    for(var i = 0; i < xs.length; i++){
        if(xs[i].index === index){
            xs[i] = value;
            return;
        }
    }
    xs.push(value);
};
CMap.prototype.has = function(index){
    var xs = this.array[this.hash(index)];
    for(var i = 0; i < xs.length; i++){
        if(xs[i].index === index){
            return true;
        }
    }
    return false;
};
CMap.prototype.delete = function(){
    var xs = this.array(this.hash(index));
    for(var i = 0; i < xs.length; i++){
        if(xs[i].index === index){
            xs.splice(i, 1);
            return;
        }
    }
};
CMap.prototype.collesions = function(){
    var n = 0;
    for(var i = 0; i < this.array.length; i++){
        n = Math.max(n, this.array[i].length);
    }
    return n;
};

exports.createChunkMap = function(){
    //return { map: {}, list: [] };
    return { map: new Map(), list: [] };
    //return { map: new CMap(), list: [] };
};

exports._lookup = function(index){
    return function(obj){
        return function(){
            //return obj.map[index] || null;
            return obj.map.get(index) || null;
        };
    };
};

exports._peekAt = function(intIndex){
    return function(obj){
        return function(){
            return obj.list[intIndex % obj.list.length] || null;
        };
    };
};

exports.insert = function(index){
    return function(value){
        return function(obj){
            return function(){
                //if(obj.map[index]){
                if(obj.map.has(index)){
                    var i = obj.list.findIndex(function(chunkWithMesh){
                        return chunkWithMesh.index === index;
                    });
                    if(i === -1){
                        // never come here
                        //debugger;
                        throw new Error();
                    }
                    obj.list.splice(i, 1, value);
                }else{
                    obj.list.push(value);
                }
                //obj.map[index] = value;
                obj.map.set(index, value);
            };
        };
    };
};

exports.sort = function(cx){
    return function(cy){
        return function(cz){
            return function(obj){
                return function(){
                    obj.list.sort(function(a, b){
                        var r = Math.max(Math.abs(a.x - cx), Math.abs(a.y - cy), Math.abs(a.z - cz));
                        var t = Math.max(Math.abs(b.x - cx), Math.abs(b.y - cy), Math.abs(b.z - cz));
                        return r - t;
                    });
                };
            };
        };
    };
};

exports.delete = function(index){
    return function(obj){
        return function(){
            //if(obj.map[index]){
            if(obj.map.has(index)){
                var i = obj.list.findIndex(function(chunkWithMesh){
                    return chunkWithMesh.index === index;
                });
                obj.list.splice(i, 1);
                //delete obj.map[index];
                obj.map.delete(index);
            }
        };
    };
};

exports.size = function(obj){
    return function(){
        return obj.list.length;
    };
};

exports.slice = function(begin){
    return function(end){
        return function(obj){
            return function(){
                return obj.list.slice(begin, end);
            };
        };
    };
};


exports.filterNeighbors = function(range){
    return function(cx){
        return function(cy){
            return function(cz){
                return function(obj){
                    return function(){
                        return obj.list.filter(function(a){
                            return Math.max(Math.abs(a.x - cx), Math.abs(a.y - cy), Math.abs(a.z - cz)) <= range;
                        });
                    };
                };
            };
        };
    };
};


exports.getSortedChunks = function(cx){
    return function(cy){
        return function(cz){
            return function(obj){
                return function(){
                    var sorted = obj.list.slice();
                    sorted.sort(function(a, b){
                        var r = Math.max(Math.abs(a.x - cx), Math.abs(a.y - cy), Math.abs(a.z - cz));
                        var t = Math.max(Math.abs(b.x - cx), Math.abs(b.y - cy), Math.abs(b.z - cz));
                        return r - t;
                    });
                    return sorted;
                };
            };
        };
    };
};