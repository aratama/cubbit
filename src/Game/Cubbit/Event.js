exports.onMouseMove = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousemove", function(e){
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
        });
    }
}

exports.onKeyUp = function(callback){
    return function(){
        window.addEventListener("keyup", function(e){
            callback(e)();
        });
    }
}


exports.focus = function(id){
    return function(){
        document.getElementById(id).focus();
    }
}