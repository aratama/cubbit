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

exports.saveChunkToFirebase = function(chunk) {
    return function() {
        var xs = "";
        for(var i = 0; i < chunk.blocks.length; i++){
            xs += String.fromCharCode(chunk.blocks[i]);
        }
        var compressed = LZString.compressToUTF16(xs);
        var database = firebase.database();
        var ref = firebase.database().ref().child("terrain").child(chunk.index);
        ref.set(compressed);

    };
};

exports.listenAllChunksFromForebase = function(callback) {
    return function() {
        var database = firebase.database();
        var ref = firebase.database().ref().child("terrain");
        ref.once("value").then(function(snap){
            snap.forEach(function(chunkSnap){
                var decompressed = LZString.decompressFromUTF16(chunkSnap.val().replace());
                if(decompressed == null){
                    debugger;
                    console.error("invalid chunk data at " + chunkSnap.key);
                }else{
                    var ys = Uint8Array.from({ length: 4096 }, function(v, i){
                        return decompressed.charCodeAt(i);
                    });
                    callback({
                        index: parseInt(chunkSnap.key),
                        blocks: ys
                    })();
                }
            })
        });
    };
};