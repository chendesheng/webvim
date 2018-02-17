const fs = require('fs');

const placeholder = '<!-- inject index.js -->';
const htmlfile = fs.readFileSync('build/index.html', { encoding : 'utf8' });
const jsfile = fs.readFileSync('index.js', { encoding : 'utf8' });
fs.writeFileSync('index.html', htmlfile.replace(placeholder, jsfile));
