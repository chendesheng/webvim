const gulp = require('gulp');
const spawn = require('child_process').spawn;
const {genTests} = require('./tests/gen/gen.js');
const {generateCss} = require('./build/less.config.js');
const {generateHtml} = require('./build/html.config.js');
const http = require('http');
const browserSync = require('browser-sync').create();
const AsyncLock = require('async-lock');
const os = require('os');
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
    const home = os.homedir();
    return exec(['ctags',
      '-R',
      '--fields=+n',
      '--exclude="*.json"',
      `--exclude="${home}/.elm/0.19.0/package/elm/**/tests"`,
      '--exclude="tests/elm-stuff/pac-community/elm-test/**/benchmarks"',
      '--exclude="tests/gen/**"',
      'src',
      'tests',
      `${home}/.elm`,
    ].join(' '));
  });
});

gulp.task('genTests', genTests);

gulp.task('css', generateCss);
gulp.task('html', generateHtml);
gulp.task('js', function() {
  return lock.acquire('elm make', function() {
    return exec('elm make src/Main.elm --output dist/elm.js');
  });
});

gulp.task('test', async function() {
  await genTests();
  await exec('elm-test');
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
  gulp.watch(['tests/gen/data/*.*', 'tests/gen/gen.js'],
    watchOptions, gulp.parallel('genTests'));
});


