const webfontsGenerator = require('webfonts-generator');
const CleanCSS = require('clean-css');
const path = require('path');
const fs = require('fs');

const folder = './css';

webfontsGenerator({
  files: fs.readdirSync(path.join(folder, 'icons'))
    .map(file => path.join(folder, 'icons', file)),
  dest: path.join(folder, 'fonts'),
  fontName: 'icons',
  cssDest: path.join(folder, 'Icon.less'),
  html: true,
  // htmlTemplate: './build/font/html-template.hbs',
  htmlDest: path.join(folder, 'preview.html'),
  baseClass: 'icon',
  cssFontsUrl: '../css/fonts',
  types: ['eot', 'woff', 'ttf', 'svg'],
  cssTemplate: './build/font/css-template.hbs',
  // writeFiles: false,
}, (error, result) => {
  if (error) {
    console.error(error);
    return;
  }
  const css = result.generateCss();
  const file = path.join(folder, 'Icon.less');
  fs.writeFileSync(file, css, { encoding: 'utf8' });
  console.log('write ' + file);
});
