const theme = require('./parseTheme.js');
const registry = theme.registry;
const fs = require('fs');
const path = require('path');

const Hapi=require('hapi');


// Create a server with a host and port
const server=Hapi.server({
    host: '0.0.0.0',
    port: 8765,
});

const allCaches = {}; // cache StackElement

function walk(dir, callback) {
  fs.readdir(dir, function(err, files) {
    if (err) throw err;
    files.forEach(function(file) {
      let filepath = path.join(dir, file);
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
  'rb': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/ruby/syntaxes/ruby.tmLanguage.json'),
  'sh': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/shellscript/syntaxes/shell-unix-bash.tmLanguage.json'),
  'ps': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/powershell/syntaxes/powershell.tmLanguage.json'),
  'm': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/objective-c/syntaxes/objective-c.tmLanguage.json'),
  'clj': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/clojure/syntaxes/clojure.tmLanguage.json'),
  'cljs': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/clojure/syntaxes/clojure.tmLanguage.json'),
};

const loadAllGrammars = (dir) => {
  walk(dir, (filename) => {
    if (/[.]tmLanguage[.]json$/i.test(filename)) {
      const grammar = registry.loadGrammarFromPathSync(filename);
      const name = path.basename(filename).split('.')[0];
      // console.log('load grammar:', filename, name);
      allGrammars[name.toLowerCase()] = grammar;
    }
  });
};

loadAllGrammars('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions');
console.log(Object.keys(allGrammars));
/* eslint-enable max-len */


const getGrammar = (p) => {
  // console.log(p);
  // console.log(path.extname(p));
  return allGrammars[path.extname(p).substr(1).toLowerCase()];
};

const setCORSHeader = (h) =>
  h.header('Access-Control-Allow-Methods', '*')
   .header('Access-Control-Allow-Origin', '*');

removeDuplicates = (arr) => {
  const x = arr.filter((a)=>a.length > 0).reduce((result, s) => {
    return s.split(/\s+/).reduce((result, s) => {
      result[s] = 0;
      return result;
    }, result);
  }, {});
  return Object.keys(x);
};

// Add the route
server.route({
  method: 'POST',
  path: '/tokenize',
  handler: function(request, h) {
    try {
      if (!request.query.line) throw new Error('line is required');
      if (!/^\d+$/.test(request.query.line)) {
        throw new Error('line must be a number');
      }
      if (!request.query.version) throw new Error('version is required');
      if (!/^\d+$/.test(request.query.version)) {
        throw new Error('version must be a number');
      }
      if (!request.query.path) throw new Error('path is required');

      const cache = allCaches[request.query.path] || [null];
      const begin = parseInt(request.query.line);
      const lines = request.payload.split(/^/m);
      const result = [];
      const grammar = getGrammar(request.query.path);
      // console.log('tokenize:', request.query.path)
      // console.log('lines.length:', lines.length);
      // console.log('cache.length:', cache.length);
      // console.log('request.query.line:', request.query.line);
      // console.log('request.query.version:', request.query.version);

      for (let i = 0; i < lines.length; i++) { // lines.length
        const line = lines[i];
        const n = begin + i;
        if (cache.length <= n && n > 0) {
          console.log('cache miss: n='+n+', cache.length='+cache.length);
          delete allCaches[request.query.path];
          return setCORSHeader(h
            .response({type: 'error', payload: 'cacheMiss'})
            .code(200)
            .type('text/json'));
        }
        const r = grammar.tokenizeLine2(line, cache[n]);
        const tokens = Array.from(r.tokens);
        tokens.push(line.length);
        tokens.push(0);
        result[i] = tokens;
        // console.log('Line: #' + i + ', tokens: ' + r.tokens);
        cache[n+1] = r.ruleStack;
      }
      allCaches[request.query.path] = cache.slice(0, begin + lines.length + 1);
      // console.log(result);
      return setCORSHeader(
        h.response({
          type: 'success',
          payload: result,
          version: parseInt(request.query.version),
          path: request.query.path,
        })
          .code(200)
          .type('text/json'));
    } catch (e) {
      console.error(e);
    }
  },
});

server.route({
  method: 'GET',
  path: '/css',
  handler: function(request, h) {
    return h.response(theme.css).type('text/css').code(200);
  },
});

server.route({
  method: 'GET',
  path: '/kill',
  handler: function(request, h) {
    process.exit(0);
  },
});

// Start the server
async function start() {
  try {
    await server.start();
  } catch (err) {
    console.log(err);
    process.exit(1);
  }

  console.log('Server running at:', server.info.uri);
};

start();
