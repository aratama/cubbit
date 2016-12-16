exports.loadImageEff = function(url){
    return function(reject){
        return function(resolve){
            return function(){
                var image = new Image();
                image.onload = function(){
                    resolve(image)();
                };
                image.onerror = function(e){
                    reject(new Error("loadImage: " + url))();
                }
                image.src = url;
            }
        }
    }
}