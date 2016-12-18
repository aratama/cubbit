var zip = require('bestzip');
var glob = require('glob');
var rimraf = require('rimraf');
var fs = require('fs');
var cpx = require("cpx");

rimraf('./docs', function(){
    cpx.copy("./public/**/*@(.js|.html|.css|.png|.jpg|.ttf|.otf|.woff|.txt|.json|.mp3|.svg)", "docs", {}, function(){
        glob("./public/**/*@(.babylon)", {}, function (er, files) {
            files.forEach(function(file){
                var dat = fs.readFileSync(file);
                fs.writeFile(file.replace(".babylon", ".json").replace("public", "docs"), dat);
            });
            glob("./public/**/*@(.fx)", {}, function (er, files) {
                files.forEach(function(file){
                    var dat = fs.readFileSync(file);
                    fs.writeFile(file.replace(".fx", ".txt").replace("public", "docs"), dat);
                });
                glob("./public/**/*@(.js)", {}, function (er, files) {
                    files.forEach(function(file){
                        var dat = fs.readFileSync(file, 'utf8');
                        fs.writeFile(file.replace("public", "docs"), dat.replace(/".vertex.fx"/g, `".vertex.txt"`).replace(/".fragment.fx"/g, `".fragment.txt"`).replace(/\.babylon/g, ".json"));
                    });
                    zip('./nicogame.zip', "./docs", function(err) {

                    });
                });
            });
        })
    });
});

