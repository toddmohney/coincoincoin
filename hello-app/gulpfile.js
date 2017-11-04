var gulp = require('gulp');
var source = require('vinyl-source-stream');
var babelify = require("babelify");
var browserify = require('browserify');
var watchify = require('watchify');
var reactify = require('reactify');
var streamify = require('gulp-streamify');

var path = {
  HTML: 'src/index.html',
  MINIFIED_OUT: 'build.min.js',
  OUT: 'build.js',
  DEST: 'public',
  DEST_BUILD: 'public/javascripts',
  DEST_SRC: 'build/src',
  ENTRY_POINT: './src/js/Main.js'
};

const bundler = browserify({
  entries: [path.ENTRY_POINT],
  transform: [babelify.configure({
    presets: ["stage-0", "es2015", "react"]
  })],
});

gulp.task('copy', function(){
  gulp.src(path.HTML)
    .pipe(gulp.dest(path.DEST));
});

gulp.task('build', function(){
  doBundle(bundler);
});

gulp.task('watchify', function() {
  const watcher = watchify(bundler);
  doBundle(watcher);

  return watcher.on('update', function() {
    return doBundle(watcher);
  });
});

gulp.task('production', ['build']);

gulp.task('default', ['production']);

function doBundle(bundler) {
  return bundler.bundle()
    .pipe(source(path.MINIFIED_OUT))
    .pipe(gulp.dest(path.DEST_BUILD));
}
