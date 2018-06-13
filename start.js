const chokidar = require('chokidar');
const browserSync = require('browser-sync').create();
// const debounce = require('debounce');
const http = require('http');
const {spawn} = require('child_process');
const fs = require('fs');
const {deepEqual} = require('assert');

const executor = (() => {
  const executing = {};
  const run = (cmd, onsuccess) => {
    if (executing[cmd]) {
      console.log('kill ' + cmd);
      executing[cmd].kill();
      executing[cmd].removeAllListeners();
      executing[cmd] = null;
    }
    const args = cmd.split(/\s+/);
    const child = spawn(args[0], args.slice(1), {stdio: 'inherit'});
    child.once('exit', () => {
      const exitCode = executing[cmd].exitCode;
      console.log('exit ' + cmd);
      console.log('exitCode ' + exitCode);
      if (exitCode === 0) {
        executing[cmd] = null;
        if (onsuccess) onsuccess();
      }
    });
    child.on('error', (err) => {
      console.error(`exec error: ${err}`);
    });
    executing[cmd] = child;
  };
  return {
    run,
  };
})();

browserSync.init({
  server: {
    baseDir: './',
  },
  watchOptions: {
    ignoreInitial: true,
    ignored: '*.js',
  },
});

const shell = (sh, onsuccess) => {
  console.log(sh);
  return executor.run(sh, onsuccess);
};

const shellAndReload = (cmd, file) => shell(cmd, () => {
  console.log('reload', file);
  if (file) browserSync.reload(file);
  else browserSync.reload();
});

const jsTask = () => shellAndReload('npm run js --silent');
const cssTask = () => shellAndReload('npm run css', 'dist/style.min.css');
const ctagsTask = () => shell([
  'ctags -R --fields=+n',
  '--exclude="tests/elm-stuff"',
  '--exclude="*.json"',
  '--exclude="elm-stuff/**/tests"',
  '--exclude="elm-stuff/**/benchmarks"',
  '--exclude="tests/elm-stuff/packages/elm-community/elm-test/**/tests"',
  '--exclude="tests/elm-stuff/packages/elm-community/elm-test/**/benchmarks"',
  'src',
  'tests',
  'elm-stuff/packages',
  'tests/elm-stuff/packages/elm-community/elm-test',
  ].join(' '));

const exitTask = () => {
  // check current file syntax before restart!!
  const child = spawn('node', ['-c', __filename], {stdio: 'inherit'});
  child.on('exit', () => {
    if (child.exitCode === 0) {
      process.exit(0);
    }
  });
};
const htmlTask = () => shellAndReload('npm run html');
const fontTask = () => shellAndReload('npm run font');
const npmInstall = (() => {
  const readPackageJson = () =>
    JSON.parse(fs.readFileSync('./package.json', {encoding: 'utf8'}));
  let lastPackageJson = readPackageJson();
  return () => {
    const obj = readPackageJson();
    try {
      deepEqual(obj.dependencies, lastPackageJson.dependencies);
      deepEqual(obj.devDependencies, lastPackageJson.devDependencies);
    } catch (e) {
      shell('npm install');
    } finally {
      lastPackageJson = obj;
    }
  };
})();

// const testTask = () => shell('elm test');

const withColor = (number, str) => `\x1b[${number}m${str}\x1b[0m`;
const green = 32;

const runTaskList = (tasks) => {
  console.log(withColor(green, `${new Date}`));
  const todo = [];
  // remove duplicate tasks
  // use reverse to leave most recent one
  tasks.slice().reverse().forEach((task) => {
    if (todo.indexOf(task) === -1) {
      todo.push(task);
    }
  });

  todo
    .reverse()
    .forEach((f) => f());
  taskList = [];
};

let taskList = [];
const watch = (path, tasks) => {
  chokidar.watch(path).on('change', (event, path) => {
    taskList = taskList.concat(tasks);
    runTaskList(taskList);
  });
};

runTaskList([jsTask, cssTask, htmlTask, ctagsTask]);

watch('src/**/*.elm', [ctagsTask, jsTask]);
watch('src/Native/*.js', [jsTask]);
watch([
  'build/template.html',
  'build/index.js',
  'build/html.config.js',
], [htmlTask]);
watch(['css/**/*.less'], [cssTask]);
watch(['build/font/font-generator.js', 'css/icons/*.svg'], [fontTask]);
watch(['start.js', 'elm-package.json'], [exitTask]);
watch(['tests/**/*.elm'], [ctagsTask]);
watch('package.json', [npmInstall]);

const startBackend = (port) => {
  http.get(`http://localhost:${port}/kill`)
    .once('error',
      () => spawn('npm', ['run', 'purs:watch'], {stdio: 'inherit'})
    );
};
startBackend(8899);

