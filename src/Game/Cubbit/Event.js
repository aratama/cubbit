exports.focus = function(id){
    return function(){
        document.getElementById(id).focus();
    }
}