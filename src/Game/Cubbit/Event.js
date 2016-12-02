exports.onMouseMove = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousemove", function(e){
            callback(e)();
        });
    }
}

exports.onMouseDown = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousedown", function(e){
            callback(e)();
        });
    }
}

exports.onMouseClick = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("click", function(e){
            callback(e)();
        });
    }
}

exports.onButtonClick = function(id){
    return function(callback){
        return function(){
            document.getElementById(id).addEventListener("click", function(){
                callback();
            });
        }
    }
}

exports._wait = function(reject){
    return function(resolve){
        return function(){
            setTimeout(function(){
                resolve({})();
            }, 0);
        }
    }
}

exports.onKeyDown = function(callback){
    return function(){
        window.addEventListener("keydown", function(e){
            callback(e)();
            e.preventDefault();
            e.stopPropagation();
            if(e.keyCode == 27){
                document.exitPointerLock();
            }
        });
    }
}

exports.onKeyUp = function(callback){
    return function(){
        window.addEventListener("keyup", function(e){
            callback(e)();
            e.preventDefault();
            e.stopPropagation();
        });
    }
}


exports.focus = function(id){
    return function(){
        document.getElementById(id).focus();
    }
}