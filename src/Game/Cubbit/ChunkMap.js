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