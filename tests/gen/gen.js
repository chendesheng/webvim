const fs = require('fs-extra');
const path = require('path');
const {execSync} = require('child_process');

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

exports.genTests = async function() {
  const dirs = await fs.readdir(path.join(__dirname, 'data'));
  const code = dirs.map(function(f) {
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
  return fs.writeFile(path.join(__dirname, '../TestData.elm'), format(`
module TestData exposing(..)
import Test exposing (..)
import TestGenerated exposing (genTest)

suite : Test
suite =
    describe "test generated" <|
        [${code}
        ]
`));
};


