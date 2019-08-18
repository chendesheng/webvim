const gulp = require('gulp');
const spawn = require('child_process').spawn;
const {genTests} = require('./tests/gen/gen.js');
const {convertLess} = require('./build/less.config.js');
const {generateHtml} = require('./build/html.config.js');
const http = require('http');
const browserSync = require('browser-sync').create();
const AsyncLock = require('async-lock');
const os = require('os');
const lock = new AsyncLock();
const fs = require('fs');

function log(s) {
  console.log(`[${new Date()}]: ${s}`);
}

function exec(cmd) {
  log(cmd);
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
      `--exclude="${home}/.elm/0.19.1-beta-1/package/elm/**/tests"`,
      '--exclude="tests/elm-stuff/pac-community/elm-test/**/benchmarks"',
      '--exclude="tests/gen/**"',
      'src',
      'tests',
      `${home}/.elm`,
    ].join(' '));
  });
});

gulp.task('genTests', genTests);

function lastArg() {
  return process.argv[process.argv.length - 1].toLowerCase();
}

const noDebugger = lastArg() === '--nodebugger';

gulp.task('css',
  () => convertLess(noDebugger ? './css/style.less' : './css/debugger.less',
    './dist/style.min.css'));

gulp.task('html', generateHtml);
gulp.task('js', function() {
  return lock.acquire('elm make', function() {
    const __debugger = noDebugger ? '' : '--debug ';
    return exec(`elm make src/Main.elm ${__debugger}--output dist/elm.js`);
  });
});

gulp.task('test', async function() {
  // I can't get elm-test work under elm 0.19.1
  // use different elm.json and elm compiler for testing
  fs.renameSync('elm.json', '.elm.json');
  fs.renameSync('tests-elm.json', 'elm.json');
  await genTests();

  // change elm.json back after 1 second
  setTimeout(() => {
    fs.renameSync('elm.json', 'tests-elm.json');
    fs.renameSync('.elm.json', 'elm.json');
  }, 3000);
  await exec('elm-test --compiler=/usr/local/bin/elm19');
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

  gulp.series('js', 'css')();
});


