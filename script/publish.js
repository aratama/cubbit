var copy = require('copy');
copy('public/**/*.*', 'docs', function(err, files) {
  if (err) throw err;
  // `files` is an array of the files that were copied 
});