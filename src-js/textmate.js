const Registry = require('vscode-textmate').Registry;
const theme = require('./parseTheme.js');
const registry = theme.registry;
// const registry = new Registry({
//   theme: theme.raw,
//   getFilePath: function (scopeName) {
//     const dir = '/Applications/Visual Studio Code.app/Contents/Resources/app/extensions';
// 		// Return here the path to the grammar file for `scopeName`
// 		if (scopeName === 'source.js') {
// 			return `${dir}/javascript/Syntaxes/JavaScript.tmLanguage.json`;
// 		} else if (scopeName === 'source.jsx') {
// 			return `${dir}/javascript/Syntaxes/JavaScriptReact.tmLanguage.json`;
// 		} else if (scopeName === 'source.py') {
// 			return `${dir}/python/Syntaxes/MagicPython.tmLanguage.json`;
// 		} else if (scopeName === 'source.regex.py') {
// 			return `${dir}/python/Syntaxes/MagicRegExp.tmLanguage.json`;
//     }
// 
//     const name = path.extname(scopeName).substr(1);
//     console.log('load file:', `${dir}/${name}/syntaxes/${name}.tmLanguage.json`);
//     return `${dir}/${name}/syntaxes/${name}.tmLanguage.json`;
// 	}
// });
const fs = require('fs');
const path = require('path');

const Hapi=require('hapi');

// var grammar = registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/less/syntaxes/less.tmLanguage.json');
// 
// var lineTokens = grammar.tokenizeLine('@a:1.2rem;');
// console.log(lineTokens);
// for (var i = 0; i < lineTokens.tokens.length; i++) {
// 	var token = lineTokens.tokens[i];
// 	console.log('Token from ' + token.startIndex + ' to ' + token.endIndex + ' with scopes ' + token.scopes);
// }

// var lines = fs.readFileSync('dist/elm.js', { encoding: 'utf-8' }).split('\n');
// 
// console.time("tokenize");
// 
// var ruleStack = null;
// for (var i = 0; i < lines.length; i++) { // lines.length
// 	var r = grammar.tokenizeLine(lines[i], ruleStack);
//   // console.log('Line: #' + i + ', tokens: ' + r.tokens);
// 	ruleStack = r.ruleStack;
// }
// 
// console.timeEnd("tokenize");


// tokenize first 10 lines:
// POST /tokenize?line=10&path=src/main.elm
//  response: 1. success, 2. failed, 3. cache lost
// type alias Token = { scope : String, region: (Int, Int) }
// type alias Syntax = Array (List Token)
//   when syntax changed, check if scrollTop ~ scrollTop + height is still valid
//      if not , send request to update syntax
//      else no change
// getTokens syntax by ey =
//
//
//

// Create a server with a host and port
const server=Hapi.server({
    host:'0.0.0.0',
    port:8765
});

const allCaches = {}; // cache StackElement

function walk(dir, callback) {
	fs.readdir(dir, function(err, files) {
		if (err) throw err;
		files.forEach(function(file) {
			var filepath = path.join(dir, file);
			fs.stat(filepath, function(err,stats) {
				if (stats.isDirectory()) {
					walk(filepath, callback);
				} else if (stats.isFile()) {
					callback(filepath, stats);
				}
			});
		});
	});
}

const allGrammars = {
  'js': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/javascript/syntaxes/JavaScript.tmLanguage.json'),
  'jsx': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/javascript/syntaxes/JavaScriptReact.tmLanguage.json'),
  'fs': registry.loadGrammarFromPathSync('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/fsharp/syntaxes/fsharp.tmLanguage.json'),
  'elm': registry.loadGrammarFromPathSync('/Users/chendesheng/.vscode/extensions/sbrink.elm-0.16.0/syntaxes/elm.json'),
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
      
      console.log('load grammar:', filename);
      allGrammars[path.basename(filename).split('.')[0]] = registry.loadGrammarFromPathSync(filename);
    }
  });
};

loadAllGrammars('/Applications/Visual Studio Code.app/Contents/Resources/app/extensions');
console.log(Object.keys(allGrammars));


const loadGrammar = (p) => new Promise((resolve, reject) => {
  let scopeName = 'source' + path.extname(p);
  const scopeNameMapper = { 'source.htm': 'source.html' };
  scopeName = scopeNameMapper[scopeName] || scopeName;

  registry.loadGrammar(scopeName, (err, grammar) => {
    if (err) {
      reject(err);
    }

    console.log('grammar:', grammar);
    resolve(grammar);
  });
});

const getGrammar = (p) => {
  // console.log(p);
  // console.log(path.extname(p));
  return allGrammars[path.extname(p).substr(1)];
};

const setCORSHeader = h => 
  h.header('Access-Control-Allow-Methods', '*')
   .header('Access-Control-Allow-Origin', '*')

removeDuplicates = arr => {
  const x = arr.filter(a=>a.length > 0).reduce((result, s) => {
    return s.split(/\s+/).reduce((result, s) => {
      result[s] = 0;
      return result;
    }, result);
  }, {});
  return Object.keys(x);
};

// Add the route
server.route({
  method:'POST',
  path:'/tokenize',
  handler: function(request,h) {
    try {
      if (!request.query.line) throw new Error('line is required');
      if (!/^\d+$/.test(request.query.line)) {
        throw new Error('line must be a number');
      }
      if (!request.query.path) throw new Error('path is required');

      const cache = allCaches[request.query.path] || [];
      const begin = parseInt(request.query.line);  
      const lines = request.payload.split(/^/m);
      const result = [];
      const grammar = getGrammar(request.query.path);
      console.log('tokenize:', request.query.path)
      console.log ('lines.length:', lines.length);
      console.log('cache.length:', cache.length);
      console.log('request.query.line:', request.query.line);
      for (let i = 0; i < lines.length; i++) { // lines.length
        const line = lines[i];
        const n = begin + i;
        if (cache.length <= n && n > 0) {
          delete allCaches[request.query.path];
          return setCORSHeader(h
            .response({ type: 'error', payload: 'cacheMiss' })
            .code(200)
            .type('text/json'));
        }
        const r = grammar.tokenizeLine2(line, cache[n]);
        const tokens = Array.from(r.tokens)
        tokens.push(line.length);
        tokens.push(0);
        result[i] = tokens;
        // console.log('Line: #' + i + ', tokens: ' + r.tokens);
        cache[n+1] = r.ruleStack;
      }
      allCaches[request.query.path] = cache.slice(0, begin + lines.length);
      // console.log(result);
      return setCORSHeader(
        h.response({ type: 'success', payload: result })
          .code(200)
          .type('text/json'));
    } catch (e) {
      console.error(e);
    }
  }
});

const cssfile = 
server.route({
  method:'GET',
  path:'/css',
  handler: function(request,h) {
    return h.response(theme.css).type('text/css').code(200)
  },
});

server.route({
  method:'GET',
  path:'/kill',
  handler: function(request,h) {
    process.exit(0);
  },
});

// Start the server
async function start() {
  try {
    await server.start();
  }
  catch (err) {
    console.log(err);
    process.exit(1);
  }

  console.log('Server running at:', server.info.uri);
};

start();
