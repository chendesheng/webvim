const fs = require('fs');
const json = require('comment-json');
const Registry = require('vscode-textmate').Registry;

const file = fs.readFileSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/theme-monokai/themes/monokai-color-theme.json',
  { encoding: 'utf-8' }
);
const theme = json.parse(file);

const getDefaultSettings = (tokenColors) => {
  const defaults = tokenColors.find(({scope, settings})=> scope)
  return defaults && defaults.settings;
};

const defaultForeground = getDefaultSettings(theme.tokenColors).foreground;
// console.log('defaultForeground:', defaultForeground);


const colors = [];
theme.tokenColors.forEach(({scope,settings})=> {
  if (scope) {
    const color = settings.foreground || defaultForeground;
    if (colors.indexOf(color) === -1) {
      colors.push(color);
    }
  }
});

const registry = new Registry();
registry.setTheme({
  name: 'Monokai',
  settings: theme.tokenColors,
});


const css = registry.getColorMap().map((color, i) => {
  return `.mtk${i}{color:${color}}\n`;
}).join('')
  + '.mtki{font-style:italic}\n'
  + '.mtkb{font-weight:bold}\n';
  + '.mtku{border-bottom: solid 1px}';

module.exports = {
  colors,
  defaultColor: defaultForeground,
  registry,
  css,
};

