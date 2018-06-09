const fs = require('fs');

const getLineHeight = () => {
  try {
    return parseInt(fs.readFileSync('css/style.less', {encoding: 'utf8'})
      .match(/@line-height:\s?(\d+)px;/)[1]);
  } catch (e) {
    console.warn('read line height failed: ' + e);
    return 21;
  }
};

const base64Encode = (file) => {
  const image = fs.readFileSync(file);
  return new Buffer(image).toString('base64');
};

const placeholder = '<!-- inject index.js -->';
const htmlfile = fs.readFileSync('build/template.html', {encoding: 'utf8'});
const jsfile
  = `const lineHeight = ${getLineHeight()};\n`
  + fs.readFileSync('build/index.js', {encoding: 'utf8'});
fs.writeFileSync('index.html',
  htmlfile
    .replace(placeholder, jsfile)
    .replace('href="favicon.ico"',
             `href="data:image/x-icon;base64,${base64Encode('favicon.ico')}"`)
);

