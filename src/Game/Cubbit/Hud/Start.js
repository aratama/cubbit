exports.reportToSentry = function(){
    setTimeout(function(){
        Raven.captureException(new Error('Cubbit may be going well.'), {
            level: 'info'
        });
    }, 5000);
    exports.reportToSentry = function(){};
};