exports.createChunkMap = function(){
    return { size: 0, map: {} };
}

exports._lookup = function(index){
    return function(obj){
        return function(){
            return obj.map[index] || null;
        }
    }
}

exports.insert = function(index){
    return function(value){
        return function(obj){
            return function(){
                if( ! obj.map[index]){
                    obj.size += 1;
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
                obj.size -= 1;
            }
            delete obj.map[index];
        }
    }
}

exports.size = function(obj){
    return function(){
        return obj.size;
    }
}