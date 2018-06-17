const fs = require('fs');
const json = require('comment-json');
const Registry = require('vscode-textmate').Registry;
const path = require('path');

/* eslint-disable no-var */

/* eslint-disable max-len */
// const file = fs.readFileSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/theme-solarized-dark/themes/solarized-dark-color-theme.json',
const file = fs.readFileSync('build/theme.json', {encoding: 'utf-8'});
/* eslint-enable max-len */
const theme = json.parse(file);
const registry = new Registry();
registry.setTheme({
  name: theme.label,
  settings: theme.tokenColors,
});

const allCaches = {}; // cache StackElement

function walk(dir, callback) {
  fs.readdir(dir, function(err, files) {
    if (err) throw err;
    files.forEach(function(file) {
      var filepath = path.join(dir, file);
      fs.stat(filepath, function(err, stats) {
        if (stats.isDirectory()) {
          walk(filepath, callback);
        } else if (stats.isFile()) {
          callback(filepath, stats);
        }
      });
    });
  });
}

/* eslint-disable max-len */
const allGrammars = {
  'js': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/javascript/syntaxes/JavaScript.tmLanguage.json'),
  'jsx': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/javascript/syntaxes/JavaScriptReact.tmLanguage.json'),
  'fs': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/fsharp/syntaxes/fsharp.tmLanguage.json'),
  'elm': registry.loadGrammarFromPathSync('/Users/chendesheng/.vscode/extensions/sbrink.elm-0.18.0/syntaxes/elm.json'),
  'purs': registry.loadGrammarFromPathSync('/Users/chendesheng/.vscode/extensions/nwolverson.language-purescript-0.1.2/syntaxes/purescript.json'),
  'rb': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/ruby/syntaxes/ruby.tmLanguage.json'),
  'sh': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/shellscript/syntaxes/shell-unix-bash.tmLanguage.json'),
  'ps': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/powershell/syntaxes/powershell.tmLanguage.json'),
  'm': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/objective-c/syntaxes/objective-c.tmLanguage.json'),
  'clj': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/clojure/syntaxes/clojure.tmLanguage.json'),
  'cljs': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/clojure/syntaxes/clojure.tmLanguage.json'),
};

function loadAllGrammars(dir) {
  walk(dir, function(filename) {
    if (/[.]tmLanguage[.]json$/i.test(filename)) {
      const grammar = registry.loadGrammarFromPathSync(filename);
      const name = path.basename(filename).split('.')[0];
      // console.log('load grammar:', filename, name);
      allGrammars[name.toLowerCase()] = grammar;
    }
  });
};

loadAllGrammars('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions');
// console.log(Object.keys(allGrammars));

function getGrammar(p) {
  // console.log(p);
  // console.log(path.extname(p));
  return allGrammars[path.extname(p).substr(1).toLowerCase()];
};

exports.tokenize =
  function(path) {
    return function(line) {
      return function(payload) {
        return function() {
          try {
            const cache = allCaches[path] || [null];
            const begin = parseInt(line);
            const lines = payload.split(/^/m);
            const result = [];
            const grammar = getGrammar(path);
            // console.log('tokenize:', request.query.path)
            // console.log('lines.length:', lines.length);
            // console.log('cache.length:', cache.length);
            // console.log('request.query.line:', request.query.line);

            for (var i = 0; i < lines.length; i++) { // lines.length
              const line = lines[i];
              const n = begin + i;
              if (cache.length <= n && n > 0) {
                console.log('cache miss: n='+n+', cache.length='+cache.length);
                delete allCaches[path];
                return JSON.stringify({type: 'error', payload: 'cacheMiss'});
              }
              const r = grammar.tokenizeLine2(line, cache[n]);
              const tokens = Array.from(r.tokens);
              tokens.push(line.length);
              tokens.push(0);
              result[i] = tokens;
              // console.log('Line: #' + i + ', tokens: ' + r.tokens);
              cache[n+1] = r.ruleStack;
            }
            allCaches[path] = cache.slice(0, begin + lines.length + 1);
            return JSON.stringify({
              type: 'success',
              payload: result,
              path: path,
            });
          } catch (e) {
            console.error(e);
            return {
              type: 'error',
              payload: e.message || e.toString(),
            };
          }
        };
    };
    };
  };

