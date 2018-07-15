const gulp = require('gulp');
const spawn = require('child_process').spawn;
const {genTests} = require('./tests/gen/gen.js');
const {generateCss} = require('./build/less.config.js');
const {generateHtml} = require('./build/html.config.js');
const http = require('http');
const browserSync = require('browser-sync').create();
const AsyncLock = require('async-lock');
const lock = new AsyncLock();

function exec(cmd) {
  return new Promise(function(resolve, reject) {
    const parts = cmd.split(' ');
    const child = spawn(parts[0], parts.slice(1), {
      stdio: 'inherit',
    });
    child.on('close', (code) => {
      return code === 0 ? resolve() : reject(code);
    });
  });
}

gulp.task('ctags', async function() {
  return lock.acquire('ctags', function() {
    return exec(['ctags',
      '-R',
      '--fields=+n',
      '--exclude="tests/elm-stuff"',
      '--exclude="*.json"',
      '--exclude="elm-stuff/**/tests"',
      '--exclude="elm-stuff/**/benchmarks"',
      '--exclude="tests/elm-stuff/packages/elm-community/elm-test/**/tests"',
      '--exclude='
      + '"tests/elm-stuff/packages/elm-community/elm-test/**/benchmarks"',
      '--exclude="tests/gen/**"',
      'src',
      'tests',
      'elm-stuff/packages',
      'tests/elm-stuff/packages/elm-community/elm-test',
    ].join(' '));
  });
});

gulp.task('genTests', genTests);

gulp.task('css', generateCss);
gulp.task('html', generateHtml);
gulp.task('js', function() {
  return lock.acquire('elm make', function() {
    return exec('elm make src/Main.elm --output dist/elm.js --debug --warn');
  });
});

gulp.task('test', async function() {
  await genTests();
  await exec('elm test');
});

function startBackend(port) {
  http.get(`http://localhost:${port}/kill`)
    .once('error', () => exec('npm run purs:watch'));
}

gulp.task('default', function() {
  startBackend(8899);

  browserSync.init({
    files: ['./dist/style.min.css'],
    server: './',
  });

  const watchOptions = { };
  gulp.watch(['css/**/*.less'], watchOptions, gulp.parallel('css'));
  gulp.watch(['build/template.html', 'build/index.js'],
    watchOptions, gulp.parallel('html'));

  gulp.watch('dist/elm.js').on('change', browserSync.reload);

  gulp.watch(['src/**/*.elm'],
    watchOptions, gulp.parallel('js', 'ctags'));
  gulp.watch(['tests/**/*.elm'], gulp.parallel('ctags'));
  gulp.watch(['tests/gen/data/*.*'], watchOptions, gulp.parallel('genTests'));
});


