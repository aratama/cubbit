exports.createChunkMap = function(){
    return { map: {}, list: [] };
}

exports._lookup = function(index){
    return function(obj){
        return function(){
            return obj.map[index] || null;
        }
    }
}

exports._peekAt = function(intIndex){
    return function(obj){
        return function(){
            return obj.list[intIndex % obj.list.length] || null;
        }
    }
}

exports.insert = function(index){
    return function(value){
        return function(obj){
            return function(){
                if(obj.map[index]){
                    var i = obj.list.findIndex(function(chunkWithMesh){
                        return chunkWithMesh.blocks.index === index;
                    });
                    obj.list.splice(i, 1, value);
                }else{
                    obj.list.push(value);
                }
                obj.map[index] = value;
            }
        }
    }
}

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
                }
            }
        }
    }
}

exports.delete = function(index){
    return function(obj){
        return function(){
            if(obj.map[index]){
                var i = obj.list.findIndex(function(chunkWithMesh){
                    return chunkWithMesh.blocks.index === index;
                });
                obj.list.splice(i, 1);
                delete obj.map[index];
            }
        }
    }
}

exports.size = function(obj){
    return function(){
        return obj.list.length;
    }
}

exports.slice = function(begin){
    return function(end){
        return function(obj){
            return function(){
                return obj.list.slice(begin, end);
            }
        }
    }
}


exports.filterNeighbors = function(range){
    return function(cx){
        return function(cy){
            return function(cz){
                return function(obj){
                    return function(){
                        return obj.list.filter(function(a){
                            return Math.max(Math.abs(a.x - cx), Math.abs(a.y - cy), Math.abs(a.z - cz)) <= range
                        });
                    }
                }
            }
        }
    }
}