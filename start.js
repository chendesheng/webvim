const chokidar = require('chokidar');
const execa = require('execa');
const browserSync = require('browser-sync').create();
const debounce = require('debounce');

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

const jsTask = () => shell('npm run js --silent');
const cssTask = () => shell('npm run css');
const reloadTask = () => browserSync.reload();
const reloadCSSTask = () => browserSync.reload('dist/style.min.css');
const ctagsTask = folder => () => shell(`ctags -R ${folder}`);
const exitTask = () => process.exit(0);
const htmlTask = () => shell('npm run html');
const fontTask = () => shell('npm run font');
const testTask = () => shell('elm test');

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

  try {
    todo.reverse().forEach(f => f());
  } catch(err) {
    console.log(`run task ${todo.map(f=>f.name)} error.`);
  } finally {
    taskList = [];
  }

}, 1000);

let taskList = [];
const watch = (path, tasks) => {
  chokidar.watch(path).on('change', (event, path) => {
    taskList = taskList.concat(tasks);
    runTaskList(taskList);
  });
};

runTaskList([jsTask, cssTask, ctagsTask("src tests"), htmlTask, reloadTask]);

watch('src/**/*.elm', [jsTask, ctagsTask("src"), reloadTask]);
watch('src/Native/*.js', [jsTask, reloadTask]);
watch(['build/index.html', 'index.js'], [htmlTask, reloadTask]);
watch(['css/**/*.less'], [cssTask, reloadCSSTask]);
watch(['build/font/font-generator.js', 'css/icons/*.svg'], [fontTask]);
watch(['start.js', 'elm-package.json'], [exitTask]);
watch(['tests/**/*.elm'], [ctagsTask("tests"), testTask]);

