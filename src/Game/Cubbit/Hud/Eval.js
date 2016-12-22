exports.openDevTools = function(){
    //if(typeof process !== "undefined" && process.versions["electron"]){
    if(window["require"]){
        var electron = window["require"]('electron');
        electron.ipcRenderer.send("command", "openDevTools");
    }
};