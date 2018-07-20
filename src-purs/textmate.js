const fs = require('fs');
const Registry = require('vscode-textmate').Registry;
const parseRawGrammar = require('vscode-textmate').parseRawGrammar;
const path = require('path');
const JSON = require('comment-json');
const homedir = require('os').homedir();

/* eslint-disable no-var */


/* eslint-disable comma-dangle */

function loadVscodeExtensions(dir, allExtensions) {
  function loadExtensionData(extensionFolder, data) {
    const contributes = data.contributes;
    if (!contributes) {
      return;
    }

    if (contributes.languages) {
      contributes.languages.forEach(function(lang) {
        if (lang.extensions) {
          lang.extensions.forEach(function(ext) {
            allExtensions.languages[ext.toLowerCase()] = lang;
          });
        }
        if (lang.filenames) {
          lang.filenames.forEach(function(filename) {
            allExtensions.languages[filename.toLowerCase()] = lang;
          });
        }
      });
    }

    if (contributes.grammars) {
      contributes.grammars.forEach(function(grammar) {
        grammar.path = path.join(extensionFolder, grammar.path);
        allExtensions.grammars[grammar.language] = grammar;
        allExtensions.scopeNames[grammar.scopeName] = grammar;
      });
    }

    if (contributes.themes) {
      contributes.themes.forEach(function(theme) {
        theme.path = path.join(extensionFolder, theme.path);
        allExtensions.themes[theme.label.toLowerCase()] = theme;
      });
    }
  }

  function loadExtension(file) {
    return new Promise(function(resolve, reject) {
      var extensionFolder = path.join(dir, file);
      fs.stat(extensionFolder, function(err, stats) {
        if (stats.isDirectory()) {
          fs.readFile(path.join(extensionFolder, 'package.json'),
            'utf8', function(err, content) {
              if (err) {
                console.error('Open extension package.json failed: ' + err);
              } else {
                // console.log('Load extension:', extensionFolder);
                loadExtensionData(extensionFolder, JSON.parse(content));
              }
              // always resolve here ignore load failed
              resolve();
            }
          );
        } else {
          resolve();
        }
      });
    });
  }

  function readdir(dir) {
    return new Promise(function(resolve, reject) {
      fs.readdir(dir, function(err, files) {
        if (err) {
          reject(err);
        } else {
          resolve(files);
        }
      });
    });
  }

  return readdir(dir)
    .then(function(files) {
      return Promise.all(files.map(loadExtension));
    }).then(function() {
    return Promise.resolve(allExtensions);
  }).catch(function() {
    // ignore not exists folder
    return Promise.resolve(allExtensions);
  });
}


const vscodeExtensions = {
  // key: file extension (like .elm) or file name (like makefile)
  // value: language (id, extensions, aliases, config)
  languages: {},
  // key: languageId, value: grammar (scope name and path)
  grammars: {},
  // key: scopeName, value: grammar (scope name and path)
  scopeNames: {},
  // key: label, value theme ({label: 'Monokai', uiTheme:'vs-dark', path})
  themes: {},
// TODO: snippets
};

loadVscodeExtensions(
  path.join(__dirname, '../vscode/extensions'),
  vscodeExtensions
).then(function(allExtensions) {
  return loadVscodeExtensions(
    // vscode directory is different in dev mode
    path.join(__dirname, '../../vscode/extensions'),
    allExtensions
  );
}).then(function(allExtensions) {
  return loadVscodeExtensions(
    path.join(homedir, '.vscode/extensions'),
    allExtensions
  );
}).then(function(allExtensions) {
  const label = 'solarized dark';
  const theme = allExtensions.themes[label];
  if (theme) {
    console.log('initDefaultTheme');
    loadThemeFile(theme.path).then(function(data) {
      registry.setTheme({
        name: label,
        settings: data.tokenColors,
      });
    });
  }
}).catch(console.error);


/* eslint-enable comma-dangle */

function genThemeCss(uiTheme, theme, colorMap) {
  console.log(uiTheme);
  const rules = uiTheme == 'vs' ?
    // light
    ['body, .tip { background: #fff; color: #111 } ',
      '.gutter { color: #2b91af } ',
      '.auto-complete, .tip { box-shadow: 0 0 8px #a8a8a8 }',
      '.ruler { box-shadow: 1px 0px 0px 0px inset #d3d3d3 }',
    ] :
    // dark
    ['body, .tip { background: #000; color: #ddd } ',
      '.gutter { color: #5a5a5a } ',
      '.auto-complete, .tip { box-shadow: 0 0 8px #000 }',
      '.ruler { box-shadow: 1px 0px 0px 0px inset #5a5a5a }',
    ];

  function cssColor(selector, prop) {
    if (!theme.colors) {
      return;
    }
    const color = theme.colors[prop];
    if (color) {
      console.log('color:', color);
      rules.push(selector + ' { color:' + color + ' }');
    }
  }

  function cssBg(selector, prop) {
    if (!theme.colors) {
      return;
    }

    const bg = theme.colors[prop];
    if (bg) {
      rules.push(selector + ' { background:' + bg + ' }');
    }
  }

  function cssShadow(selector, prop, args) {
    // widget.shadow
    if (!theme.colors) {
      return;
    }
    const color = theme.colors[prop];
    if (color) {
      return rules.push(selector + ' { box-shadow:' + args + ' ' + color + '}');
    }
  }

  cssColor('body', 'editor.forground');
  cssBg('body, .tip', 'editor.background');

  cssColor('.gutter', 'editor.foreground');
  cssColor('.gutter', 'editorLineNumber.foreground');
  cssBg('.gutter', 'editor.background');
  cssBg('.gutter .line-number', 'editor.background');

  cssColor('.status', 'statusBar.foreground');
  cssBg('.status', 'statusBar.background');

  cssBg('.selections>div', 'editor.selectionBackground');

  cssBg('.cursor', 'editorCursor.foreground');

  cssBg('.highlights>div', 'editor.selectionBackground');
  cssBg('.highlights>div', 'editor.selectionHighlightBackground');

  cssBg('.guide', 'editor.lineHighlightBackground');

  cssBg('.auto-complete', 'editor.background');
  cssColor('.auto-complete>*', 'statusBar.foreground');
  cssColor('.auto-complete>*', 'editor.foreground');
  cssColor('.auto-complete>.selected', 'list.activeSelectionForeground');
  cssBg('.auto-complete>.selected', 'list.activeSelectionBackground');
  cssColor('.auto-complete .matched', 'list.highlightForeground');

  cssShadow('.auto-complete, .tip', 'widget.shadow', '0 0 8px');

  cssShadow('.ruler', 'editorRuler.foreground', '1px 0 0 0 inset');

  for (var i = 1, len = colorMap.length; i < len; i++) {
    const color = colorMap[i];
    if (i == 1) {
      rules.push('body, .tip { color: ' + color + '; }');
    }
    rules.push('.mtk' + i + ' { color: ' + color + '; }');
  }

  rules.push('.mtki { font-style: italic; }');
  rules.push('.mtkb { font-weight: 700; }');
  rules.push('.mtku { border-bottom: solid 1px }');

  return rules.join('\n');
}
;

const registry = new Registry({
  loadGrammar: function(scopeName) {
    var grammar = vscodeExtensions.scopeNames[scopeName];
    // console.log('scopeName:', scopeName);
    // console.log('grammar:', grammar);
    if (grammar) {
      return new Promise(function(resolve, reject) {
        fs.readFile(grammar.path, 'utf8', function(err, content) {
          if (err) {
            console.log('Load grammar failed:', grammar.path);
            reject('Load grammar failed:', grammar.path);
          } else {
            resolve(parseRawGrammar(content, grammar.path));
          }
        });
      });
    }
    return null;
  },
});

function getGrammar(p) {
  // console.log(Object.keys(allGrammars));
  const ext = path.extname(p).toLowerCase();
  const extension = vscodeExtensions.languages[ext];
  // console.log('extension:', extension);
  if (extension) {
    const languageId = extension.id;
    const grammar = vscodeExtensions.grammars[languageId];
    if (grammar) {
      return registry.loadGrammar(grammar.scopeName);
    } else {
      return Promise.reject('nogrammar');
    }
  } else {
    return Promise.reject('noextension');
  }
}

function readJson(file) {
  // console.log('readJson:', file);
  return new Promise(function(resolve, reject) {
    fs.readFile(file, 'utf8', function(err, content) {
      if (err) {
        reject(err);
      } else {
        resolve(JSON.parse(content));
      }
    });
  });
}

function mergeThemeData(parent, child) {
  const colors = Object.assign({}, parent.colors, child.colors);
  const tokenColors = (parent.tokenColors || [])
    .concat(child.tokenColors || []);
  return Object.assign(
    parent,
    {
      colors: colors,
      tokenColors: tokenColors,
    });
}

function loadThemeFile(file) {
  return readJson(file).then(function(data) {
    // include can be recursive
    if (data.include) {
      return loadThemeFile(path.resolve(path.dirname(file), data.include))
        .then(function(include) {
          return mergeThemeData(data, include);
        });
    } else {
      return data;
    }
  });
}


exports.loadTheme = function(label) {
  return function(callback) {
    return function() {
      const theme = vscodeExtensions.themes[label.toLowerCase()];
      console.log('loadTheme:', label);
      if (theme) {
        if (theme.data && theme.css) {
          registry.setTheme({
            name: label,
            settings: theme.data.tokenColors,
          });
          callback(theme.css)();
        } else {
          loadThemeFile(theme.path).then(function(data) {
            theme.data = data;
            registry.setTheme({
              name: label,
              settings: data.tokenColors,
            });
            theme.css = genThemeCss(
              theme.uiTheme,
              data,
              registry.getColorMap()
            );
            callback(theme.css)();
          });
        }
      } else {
        callback('');
      }
    };
  };
};

const allCaches = {}; // cache StackElement
exports.tokenize = function(path) {
  return function(line) {
    return function(payload) {
      return function(callback) {
        return function() {
          const cache = allCaches[path] || [null];
          const begin = parseInt(line);
          const lines = payload.split(/^/m);
          const result = [];
          getGrammar(path).then(function(grammar) {
            // console.log('tokenize:', request.query.path)
            // console.log('lines.length:', lines.length);
            // console.log('cache.length:', cache.length);
            // console.log('request.query.line:', request.query.line);

            for (var i = 0; i < lines.length; i++) { // lines.length
              const line = lines[i];
              const n = begin + i;
              if (cache.length <= n && n > 0) {
                console.log('cache miss: n=' + n +
                  ', cache.length=' + cache.length);
                delete allCaches[path];
                callback(JSON.stringify({
                  type: 'cacheMiss',
                }))();
              }
              const r = grammar.tokenizeLine2(line, cache[n]);
              const tokens = Array.from(r.tokens);
              tokens.push(line.length);
              tokens.push(0);
              result[i] = tokens;
              // console.log('Line: #' + i + ', tokens: ' + r.tokens);
              cache[n + 1] = r.ruleStack;
            }
            allCaches[path] = cache.slice(0, begin + lines.length + 1);
            callback(JSON.stringify({
              type: 'success',
              payload: result,
              path: path,
            }))();
          })
            .catch(function(e) {
              console.error(e);
              callback(JSON.stringify({
                type: 'error',
                payload: e.message || e.toString(),
              }))();
            });
        };
      };
    };
  };
};

