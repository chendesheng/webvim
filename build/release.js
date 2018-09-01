const execa = require('execa');
const fs = require('fs');
const compile = require('google-closure-compiler-js').compile;

const base64Encode = (file) => {
  const image = fs.readFileSync(file);
  return new Buffer(image).toString('base64');
};

const shell = (sh) => {
  console.log(sh);
  return execa.shellSync(sh, {
    stdio: 'inherit',
  });
};

const read = (f) => fs.readFileSync(f, {
  encoding: 'utf8',
});
const commit = execa.shellSync('git rev-parse HEAD').stdout;
const packagejson = JSON.parse(read('package.json'));
const version = packagejson.version;

const optimizeByGoogleClosureCompiler = (version, commit, code) => {
  console.log('Optimizing');
  const flags = {
    jsCode: [{
      src: code,
    }],
    compilationLevel: 'ADVANCED',
    warningLevel: 'VERBOSE',
  };
  return `/**
  * compiled at ${new Date()}
  * version ${version}
  * commit ${commit}
  */
  ${compile(flags).compiledCode}`;
};

const releaseFrontEnd = () => {
  const getLineHeight = () => {
    try {
      return parseInt(read('css/style.less')
        .match(/@line-height:\s?(\d+)px;/)[1]);
    } catch ( e ) {
      console.warn('read line height failed: ' + e);
      return 21;
    }
  };

  shell('elm make src/Main.elm --output dist/.bundle.js --optimize');

  const code = optimizeByGoogleClosureCompiler(
    version,
    commit,
    [read('dist/.bundle.js'),
      `const lineHeight = ${getLineHeight()};`,
      read('build/index.js')
        .replace(/\n\s*service:.*8899.*,\s*\n/, '\nservice:"",\n'),
    ].join('\n'));

  const placeholder = '<!-- inject index.js -->';
  const htmlfile = read('build/template.html');
  const css = read('dist/style.min.css');

  fs.writeFileSync('dist/webvim.html',
    htmlfile
      .replace(placeholder, '')
      .replace(/[<]link.*dist\/style.min.css.*?>/,
        () => `<style>${css}</style>`)
      .replace('src="dist/elm.js">', () => `>${code}`)
      .replace('href="favicon.ico"',
        () => ('href="data:image/x-icon;base64,'
          + `${base64Encode('favicon.ico')}"`)));

  console.log('Successfully generated webvim.html');
};

const releaseBackEnd = () => {
  shell('pulp build -O --src-path src-purs --to dist/.bundle.js');
  const code = optimizeByGoogleClosureCompiler(
    version,
    commit,
    read('dist/.bundle.js'))
    .replace(
      '"dist/webvim.html"',
      'require("path").join(__dirname,"webvim.html")');
  fs.writeFileSync(
    'dist/webvim-backend.js',
    `#!/usr/bin/env node
${code}`
  );

  console.log('Successfully generated webvim-backend.js');
};

releaseFrontEnd();
releaseBackEnd();

fs.unlinkSync('dist/.bundle.js');
