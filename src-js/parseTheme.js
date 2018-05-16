const fs = require('fs');
const json = require('comment-json');
const Registry = require('vscode-textmate').Registry;

// const file = fs.readFileSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/theme-monokai/themes/monokai-color-theme.json',
// const file = fs.readFileSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/theme-solarized-light/themes/solarized-light-color-theme.json',
const file = fs.readFileSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/theme-solarized-dark/themes/solarized-dark-color-theme.json',
  { encoding: 'utf-8' }
);
const theme = json.parse(file);

const getDefaultSettings = (tokenColors) => {
  const defaults = tokenColors.find(({scope, settings})=> scope)
  return defaults && defaults.settings;
};

const defaultForeground = getDefaultSettings(theme.tokenColors).foreground;
// console.log('defaultForeground:', defaultForeground);


const registry = new Registry();
registry.setTheme({
  name: theme.label,
  settings: theme.tokenColors,
});

const generateTokensCSSForColorMap = (colorMap) => {
	let rules = [];
	for (let i = 1, len = colorMap.length; i < len; i++) {
		const color = colorMap[i];
		rules[i] = `.mtk${i} { color: ${color}; }`;
	}
	rules.push('.mtki { font-style: italic; }');
	rules.push('.mtkb { font-weight: 400; }');
	rules.push('.mtku { border-bottom: solid 1px }');
	return rules.join('\n');
};

const css = generateTokensCSSForColorMap(registry.getColorMap());

module.exports = {
  defaultColor: defaultForeground,
  registry,
  css,
};

