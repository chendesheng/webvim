const fs = require('fs');
const less = require('less');
const LessPluginAutoPrefix = require('less-plugin-autoprefix');
const LessPluginCleanCSS = require('less-plugin-clean-css');
const browsers = require('./browserlist');

const autoPrefixPlugin = new LessPluginAutoPrefix({ browsers });
const cleanCSSPlugin = new LessPluginCleanCSS({ advanced: true });

const watchMode = process.argv.indexOf('--watch') > 0;

const convert = (inputFilename, outputFilename) => new Promise((resolve, reject) => {
  fs.readFile(inputFilename, { encoding: 'utf-8' }, (error, data) => {
    if (error) reject(error);
    else resolve(data);
  });
})
.then(data => less.render(data, { plugins: [autoPrefixPlugin, cleanCSSPlugin] }))
.then(output => new Promise((resolve, reject) => {
  fs.writeFile(outputFilename, output.css, (e) => {
    if (e) reject(e);
    else resolve();
  });
}))
.then(() => {
  console.log(`[${new Date().toJSON()}]: ${outputFilename} has been saved.`);
})
.catch((e) => {
  console.error('ERROR:', e);
});

const watch = (input, output) => {
  const watcher = fs.watch(input, 'utf-8', (event) => {
    if (event !== 'change') watcher.close();
    convert(input, output);
  });
};

if (watchMode) {
  console.log('in watch mode...');
}

const handler = watchMode ? watch : convert;

//['style', 'chat-button', 'expand-window']
['style']
  .map(name => ({ input: `./css/${name}.less`, output: `./dist/${name}.min.css` }))
  .forEach(({ input, output }) => {
    handler(input, output);
  });
