exports.reportToSentry = function(){
    if(window.location.host === "aratama.github.io"){
        setTimeout(function(){
            Raven.captureException(new Error('Cubbit may be going well.'), {
                level: 'info'
            });
        }, 10000);
    }
    exports.reportToSentry = function(){};
};