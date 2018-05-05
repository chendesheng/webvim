const execa = require('execa');
const fs = require('fs');
const hasha = require('hasha');
const compile = require('google-closure-compiler-js').compile;

const base64Encode = (file) => {
  var image = fs.readFileSync(file);
  return new Buffer(image).toString('base64');
};

const shell = (sh) => {
  return execa.shellSync(sh, { stdio: 'inherit' });
};

const read = f => fs.readFileSync(f, { encoding: 'utf8' });

const getLineHeight = () => {
  try {
    return parseInt(read('css/style.less')
      .match(/@line-height:\s?(\d+)px;/)[1]); 
  } catch (e) {
    console.warn('read line height failed: ' + e);
    return 21;
  }
};

const packagejson = JSON.parse(read('package.json'));

shell('elm make src/Main.elm --output dist/.bundle.js');
const bundlejs = read('dist/.bundle.js')
    + `\nconst lineHeight = ${getLineHeight()};\n`
    + read('build/index.js')

console.log('Optimizing');
const flags = {
  jsCode: [{
    src: read('dist/.bundle.js')
        + `\nconst lineHeight = ${getLineHeight()};\n`
        + read('build/index.js')
  }],
  compilationLevel: 'ADVANCED',
  warningLevel: 'VERBOSE',
};
const bundleout = compile(flags).compiledCode;

const bundlehash = hasha(bundleout, { algorithm: 'sha1' });
const bundlefile = `bundle.${bundlehash}.js`;
fs.writeFileSync(`dist/${bundlefile}`,
`/**
* compiled at ${new Date()}
* version ${packagejson.version}
* commit ${execa.shellSync('git rev-parse HEAD').stdout}
*/
${bundleout}`);
console.log(`Successfully generated ${bundlefile}`);

const placeholder = '<!-- inject index.js -->';
const htmlfile = read('build/template.html');

fs.writeFileSync('dist/webvim.html', 
  htmlfile
    .replace(placeholder, '')
    .replace('dist/style.min.css', 'style.min.css')
    .replace('dist/elm.js', bundlefile)
    .replace('href="favicon.ico"',
      `href="data:image/x-icon;base64,${base64Encode("favicon.ico")}"`));

console.log('Successfully generated webvim.html');
