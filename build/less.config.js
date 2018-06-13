const fs = require('fs');
const less = require('less');
const LessPluginAutoPrefix = require('less-plugin-autoprefix');
const LessPluginCleanCSS = require('less-plugin-clean-css');
const browsers = require('./browserlist');
const path = require('path');

const autoPrefixPlugin = new LessPluginAutoPrefix({browsers});
const cleanCSSPlugin = new LessPluginCleanCSS({advanced: true});


const convert = (inputFilename, outputFilename) =>
  new Promise((resolve, reject) => {
    fs.readFile(inputFilename, {encoding: 'utf-8'}, (error, data) => {
      if (error) reject(error);
      else resolve(data);
    });
  })
  .then((data) => less.render(data + genTheme(), {
    plugins: [autoPrefixPlugin, cleanCSSPlugin],
    filename: inputFilename,
  }))
  .then((output) => new Promise((resolve, reject) => {
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


['style']
  .map((name) =>
        ({input: `./css/${name}.less`, output: `./dist/${name}.min.css`}))
  .forEach(({input, output}) => {
    convert(input, output);
  });


const genTheme = () => {
  function generateTokensCSSForColorMap(colorMap) {
    const rules = [];
    for (let i = 1, len = colorMap.length; i < len; i++) {
      const color = colorMap[i];
      rules[i] = '.mtk'+i+' { color: '+color+'; }';
    }
    rules.push('.mtki { font-style: italic; }');
    rules.push('.mtkb { font-weight: 400; }');
    rules.push('.mtku { border-bottom: solid 1px }');
    return rules.join('\n');
  };

  const Registry = require('vscode-textmate').Registry;
  const file = fs.readFileSync(
    path.join(__dirname, 'theme.json'),
    {encoding: 'utf-8'}
  );
  const theme = require('comment-json').parse(file);
  const registry = new Registry();
  registry.setTheme({
    name: theme.label,
    settings: theme.tokenColors,
  });

  return generateTokensCSSForColorMap(registry.getColorMap());
};
