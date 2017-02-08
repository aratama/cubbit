
exports._getGamepads = function(){
    return Array.prototype.map.call(navigator.getGamepads ? navigator.getGamepads() : [], function(gamepad){
        if(gamepad){
            return {
                id: gamepad.id,
                index: gamepad.index,
                mapping: gamepad.mapping,
                connected: gamepad.connected,
                buttons: Array.prototype.map.call(gamepad.buttons, function(button){
                    return {
                        value: button.value,
                        pressed: button.pressed
                    }
                }),
                axes: Array.prototype.slice.call(gamepad.axes),
                timeStamp: gamepad.timeStamp
            };
        }else{
            return null;
        }
    });
};