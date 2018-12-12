const fs = require('fs-extra');
const path = require('path');
// const {execSync} = require('child_process');

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
    String.join """
"""
    [ ${code}
    ]`;
}

function isOnly(content) {
  return /^\s*##only/.test(content);
}

function genCode(files) {
  const tests = files.map(function(f) {
    const {name, ext} = path.parse(f);
    if (ext === '.txt') {
      return {
        name,
        content: fs.readFileSync(
          path.join(__dirname, 'data', f),
          {
            encoding: 'utf8',
          }),
      };
    }
    return null;
  }).filter((a) => a !== null);

  const gen = ({name, content}) => {
    const code = `genTest "${name}" (${genLines(content)})`;
    return isOnly(content) ? `only (${code})` : code;
  };

  const onlyTest = tests.find(({content}) => isOnly(content));
  if (onlyTest) {
    return gen(onlyTest);
  }

  return tests.map(gen).join('\n   , ');
}


exports.genTests = async function() {
  const files = await fs.readdir(path.join(__dirname, 'data'));
  await fs.writeFile(path.join(__dirname, '../TestData.elm'),
    `module TestData exposing(..)
import Test exposing (..)
import TestGenerated exposing (genTest)

suite : Test
suite =
    describe "test generated" <|
        [${genCode(files)}
        ]
`);
  console.log(`${new Date()} TestData.elm has been saved`);
};


