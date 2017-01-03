
exports.getGamepads = function(){
    return navigator.getGamepads ? navigator.getGamepads() :
            navigator.webkitGetGamepads ? navigator.webkitGetGamepads : [];
};