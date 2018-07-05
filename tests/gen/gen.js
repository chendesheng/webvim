const fs = require('fs');
const path = require('path');
const {execSync} = require('child_process');
const chokidar = require('chokidar');

function genLines(s) {
  const code = s.split('\n')
    // remove comments
    .filter(function(line) {
      return line[0] !== '#';
    })
    .map(function(line) {
      return `"${line.replace(/"/ig, '\\"')}"`;
    }).join('\n   , ');

  return `
    String.join "
"
    [ ${code}
    ]`;
}

function format(code) {
  return execSync('elm-format --stdin --yes', {
    cwd: __dirname,
    input: code,
  });
}

function prefixCommand(content) {
  if (/^\s*##skip/.test(content)) {
    return 'skip <| ';
  } else if (/^\s*##only/.test(content)) {
    return 'only <| ';
  }
  return '';
}

function gen() {
  const code = fs.readdirSync(path.join(__dirname, 'data')).map(function(f) {
    const {name, ext} = path.parse(f);
    if (ext === '.txt') {
      const content = fs.readFileSync(
        path.join(__dirname, 'data', f),
        {
          encoding: 'utf8',
        });
      const prefix = prefixCommand(content);
      return `${prefix}genTest "${name}" (${genLines(content)})`;
    }
    return '';
  }).filter(function(s) {
    return s.length > 0;
  }).join('\n   , ');

  // console.log(code);
  fs.writeFileSync(path.join(__dirname, '../TestData.elm'), format(`
module TestData exposing(..)
import Test exposing (..)
import TestGenerated exposing (genTest)

suite : Test
suite =
    describe "test generated" <|
        [${code}
        ]
`));
}

gen();
if (process.argv.indexOf('--watch') >= 0) {
  console.log('Watching data change...');
  chokidar.watch(path.join(__dirname, 'data')).on('change', (event, path) => {
    gen();
    console.log('[change]Written TestData.elm');
  });
  chokidar.watch(path.join(__dirname, 'data')).on('add', (event, path) => {
    gen();
    console.log('[add]Written TestData.elm');
  });
}

