const gulp = require('gulp');
const exec = require('child_process').exec;
const {genTests} = require('./tests/gen/gen.js');

gulp.task('ctags', function() {
  return exec([
    'ctags -R --fields=+n',
    '--exclude="tests/elm-stuff"',
    '--exclude="*.json"',
    '--exclude="elm-stuff/**/tests"',
    '--exclude="elm-stuff/**/benchmarks"',
    '--exclude="tests/elm-stuff/packages/elm-community/elm-test/**/tests"',
    '--exclude="tests/elm-stuff/packages/elm-community/elm-test/**/benchmarks"',
    '--exclude="tests/gen/**"',
    'src',
    'tests',
    'elm-stuff/packages',
    'tests/elm-stuff/packages/elm-community/elm-test',
  ].join(' '));
});

gulp.task('genTests', genTests);

gulp.task('default', function() {
  const watchOptions = { };
  gulp.watch(['src/**/*.elm', 'tests/**/*.elm'],
    watchOptions, gulp.parallel('ctags'));
  gulp.watch(['tests/gen/data/*.*'], watchOptions, gulp.parallel('genTests'));
});

