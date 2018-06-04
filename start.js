const chokidar = require('chokidar');
const execa = require('execa');
const browserSync = require('browser-sync').create();
const debounce = require('debounce');
const http = require('http');

browserSync.init({
  server: {
    baseDir: "./",
  },
  watchOptions: {
    ignoreInitial: true,
    ignored: '*.js',
  },
});

const shell = (sh) => {
  console.log(sh);
  return execa.shellSync(sh, { stdio: 'inherit' });
};

const asyncCommands = {};
const shellAsync = (sh) => {
  console.log(sh);
  return execa.shell(sh, { stdio: 'inherit' });
};

const jsTask = () => shell('npm run js --silent');
const cssTask = () => shell('npm run css');
const reloadTask = () => browserSync.reload();
const reloadCSSTask = () => browserSync.reload('dist/style.min.css');
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
  'tests/elm-stuff/packages/elm-community/elm-test'
  ].join(' '));
const exitTask = () => process.exit(0);
const htmlTask = () => shell('npm run html');
const fontTask = () => shell('npm run font');
const serverTask = (port) => () =>
    http.get(`http://localhost:${port}/kill`)
      .on('error', () => shellAsync('./start'));

const syntaxServerTask = (port) => () =>
    http.get(`http://localhost:${port}/kill`)
      .on('error', () => shellAsync('npm run syntaxserver'));

// const testTask = () => shell('elm test');

const withColor = (number, str) => `\x1b[${number}m${str}\x1b[0m`;
const green = 32;

const runTaskList = debounce((tasks) => {
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
    .forEach(
      f => {
        try {
          f();
        } catch(err) {
          // console.log(`run task ${todo.map(f=>f.name)} error.`);
          console.log(err);
        }
      });
  taskList = [];

}, 1000);

let taskList = [];
const watch = (path, tasks) => {
  chokidar.watch(path).on('change', (event, path) => {
    taskList = taskList.concat(tasks);
    runTaskList(taskList);
  });
};

runTaskList([jsTask, cssTask, ctagsTask, htmlTask,
  reloadTask, serverTask(8080), syntaxServerTask(8765)]);

watch('src/**/*.elm', [jsTask, ctagsTask, reloadTask]);
watch('src/Native/*.js', [jsTask, reloadTask]);
watch([
  'build/template.html',
  'build/index.js',
  'build/html.config.js'
], [htmlTask, reloadTask]);
watch(['css/**/*.less'], [cssTask, reloadCSSTask]);
watch(['build/font/font-generator.js', 'css/icons/*.svg'], [fontTask]);
watch(['start.js', 'elm-package.json'], [exitTask]);
watch(['tests/**/*.elm'], [ctagsTask]);
watch(['src-fs/**/*.fs'], [serverTask(8080) ]);
watch(['src-js/**/*.js'], [syntaxServerTask(8765)]);

