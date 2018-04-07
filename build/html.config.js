const fs = require('fs');

const getLineHeight = () => {
  try {
    return parseInt(fs.readFileSync('css/style.less', { encoding: 'utf8' })
      .match(/@line-height:\s?(\d+)px;/)[1]); 
  } catch (e) {
    console.warn('read line height failed: ' + e);
    return 21;
  }
};

const placeholder = '<!-- inject index.js -->';
const htmlfile = fs.readFileSync('build/index.html', { encoding : 'utf8' });
const jsfile
  = `const lineHeight = ${getLineHeight()};\n`
  + fs.readFileSync('build/index.js', { encoding : 'utf8' });
fs.writeFileSync('index.html', htmlfile.replace(placeholder, jsfile));

