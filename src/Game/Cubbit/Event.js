exports.onMouseMove = function(callback){
    return function(){
        document.getElementById("renderCanvas").addEventListener("mousemove", function(e){
            callback(e)();
        });
    }
}

exports.onRightMouseDrag = function(callback){
    return function(){
        var canvas = document.getElementById("renderCanvas");
        var x;
        var y;
        canvas.addEventListener("mousedown", function(e){
            x = e.clientX;
            y = e.clientY;
        });
        canvas.addEventListener("mousemove", function(e){
            if(e.buttons === 2){
                callback({
                    movementX: e.clientX - x,
                    movementY: e.clientY - y
                })();
                x = e.clientX;
                y = e.clientY;
            }
        });
    }
}

exports.onWheel = function(callback){
    return function(){
        var canvas = document.getElementById("renderCanvas");
        canvas.addEventListener("wheel", function(e){
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