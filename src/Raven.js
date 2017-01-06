exports.install = function(url){
    return function(){
        Raven.config(url).install();
    };
};