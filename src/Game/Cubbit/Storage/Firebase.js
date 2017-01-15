function getCubbitDatabase(callback){
    var request = window.indexedDB.open("CubbitDatabase", 1);
    request.onupgradeneeded = function(event) {
        var db = event.target.result;
        var objectStore = db.createObjectStore("terrain", {
            keyPath: "index"
        });
    };
    request.onerror = function(event) {
        debugger;
    };
    request.onsuccess = function(event) {
        callback(event.target.result);
    };
}

exports.saveChunk = function(chunk) {
    return function() {
        getCubbitDatabase(function(db){
            var transaction = db.transaction(["terrain"], "readwrite");
            transaction.oncomplete = function(event) {
            };
            transaction.onerror = function(event) {};
            var objectStore = transaction.objectStore("terrain");
            var request = objectStore.put(chunk);
            request.onsuccess = function(event) {};
        });
    };
};

exports.listenAllChunks = function(callback) {
    return function() {
        getCubbitDatabase(function(db){
            var transaction = db.transaction(["terrain"]);
            var objectStore = transaction.objectStore("terrain");
            objectStore.openCursor().onsuccess = function(event) {
                var cursor = event.target.result;
                if (cursor) {
                    callback(cursor.value)();
                    cursor.continue();
                } else {
                    //
                }
            };
        });
    };
};




exports.from = function(length){
    return function(f){
        return Uint8Array.from({ length: length }, function(v, i){ return f(i); });
    };
};
exports.boxelMapToString = function(boxelMap){
    return String.fromCharCode.apply(null, boxelMap);
}

exports.parseIndex = function(str){
    return parseFloat(str);
}