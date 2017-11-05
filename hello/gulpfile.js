var browserify = require("browserify");
var fs = require("fs");
var gulp = require('gulp');

gulp.task('build', function(){
  browserify("./js/src/main.js")
    .transform("babelify", {presets: ["es2017"]})
    .bundle()
    .pipe(fs.createWriteStream("./dist/assets/javascripts/main.js"));
});

gulp.task('default', ['build']);
