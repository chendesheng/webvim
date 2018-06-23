const fs = require('fs');
const Registry = require('vscode-textmate').Registry;
const path = require('path');

/* eslint-disable no-var */


// a vscode theme
/* eslint-disable comma-dangle */
const theme = {
  'name': 'iosvka',
  'tokenColors': [
    {
      'settings': {
        'background': '#1b1d1e',
        'foreground': '#cccccc'
      }
    },
    {
      'scope': ['meta.embedded', 'source.groovy.embedded'],
      'settings': {
        'background': '#002B36',
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Comment',
      'scope': 'comment',
      'settings': {
        'fontStyle': 'italic',
        'foreground': '#808080'
      }
    },
    {
      'name': 'String',
      'scope': 'string',
      'settings': {
        'foreground': '#d08928'
      }
    },
    {
      'name': 'Regexp',
      'scope': 'string.regexp',
      'settings': {
        'foreground': '#d08928'
      }
    },
    {
      'name': 'Number',
      'scope': 'constant.numeric',
      'settings': {
        'foreground': '#60aa00'
      }
    },
    {
      'name': 'Variable',
      'scope': [
        'variable.language',
        'variable.other'
      ],
      'settings': {
        'foreground': '#268BD2'
      }
    },
    {
      'name': 'Keyword',
      'scope': 'keyword',
      'settings': {
        'foreground': '#6c9ef8'
      }
    },
    {
      'name': 'Storage',
      'scope': 'storage',
      'settings': {
        'fontStyle': 'bold',
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Class name',
      'scope': [
        'entity.name.class',
        'entity.name.type'
      ],
      'settings': {
        'fontStyle': '',
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Function name',
      'scope': 'entity.name.function',
      'settings': {
        'foreground': '#b77fdb'
      }
    },
    {
      'name': 'Variable start',
      'scope': 'punctuation.definition.variable',
      'settings': {
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Embedded code markers',
      'scope': [
        'punctuation.section.embedded.begin',
        'punctuation.section.embedded.end'
      ],
      'settings': {
        'foreground': '#D30102'
      }
    },
    {
      'name': 'Built-in constant',
      'scope': [
        'constant.language',
        'meta.preprocessor'
      ],
      'settings': {
        'foreground': '#60aa00'
      }
    },
    {
      'name': 'Support.construct',
      'scope': [
        'support.function.construct',
        'keyword.other.new'
      ],
      'settings': {
        'foreground': '#60aa00'
      }
    },
    {
      'name': 'User-defined constant',
      'scope': [
        'constant.character',
        'constant.other'
      ],
      'settings': {
        'foreground': '#d02b61'
      }
    },
    {
      'name': 'Inherited class',
      'scope': 'entity.other.inherited-class',
      'settings': {
        'foreground': '#6C71C4'
      }
    },
    {
      'name': 'Function argument',
      'scope': 'variable.parameter',
      'settings': {}
    },
    {
      'name': 'Tag name',
      'scope': 'entity.name.tag',
      'settings': {
        'foreground': '#268BD2'
      }
    },
    {
      'name': 'Tag start/end',
      'scope': 'punctuation.definition.tag',
      'settings': {
        'foreground': '#657B83'
      }
    },
    {
      'name': 'Tag attribute',
      'scope': 'entity.other.attribute-name',
      'settings': {
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Library function',
      'scope': 'support.function',
      'settings': {
        'foreground': '#268BD2'
      }
    },
    {
      'name': 'Continuation',
      'scope': 'punctuation.separator.continuation',
      'settings': {
        'foreground': '#D30102'
      }
    },
    {
      'name': 'Library constant',
      'scope': 'support.constant',
      'settings': {}
    },
    {
      'name': 'Library class/type',
      'scope': [
        'support.type',
        'support.class'
      ],
      'settings': {
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Library Exception',
      'scope': 'support.type.exception',
      'settings': {
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Library variable',
      'scope': 'support.other.variable',
      'settings': {}
    },
    {
      'name': 'Invalid',
      'scope': 'invalid',
      'settings': {}
    },
    {
      'name': 'diff: header',
      'scope': [
        'meta.diff',
        'meta.diff.header'
      ],
      'settings': {
        'background': '#60aa00',
        'fontStyle': 'italic',
        'foreground': '#E0EDDD'
      }
    },
    {
      'name': 'diff: deleted',
      'scope': 'markup.deleted',
      'settings': {
        'background': '#eee8d5',
        'fontStyle': '',
        'foreground': '#dc322f'
      }
    },
    {
      'name': 'diff: changed',
      'scope': 'markup.changed',
      'settings': {
        'background': '#eee8d5',
        'fontStyle': '',
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'diff: inserted',
      'scope': 'markup.inserted',
      'settings': {
        'background': '#eee8d5',
        'foreground': '#219186'
      }
    },
    {
      'name': 'Markup Quote',
      'scope': 'markup.quote',
      'settings': {
        'foreground': '#00aa80'
      }
    },
    {
      'name': 'Markup Lists',
      'scope': 'markup.list',
      'settings': {
        'foreground': '#60aa00'
      }
    },
    {
      'name': 'Markup Styling',
      'scope': [
        'markup.bold',
        'markup.italic'
      ],
      'settings': {
        'foreground': '#D33682'
      }
    },
    {
      'name': 'Markup Inline',
      'scope': 'markup.inline.raw',
      'settings': {
        'fontStyle': '',
        'foreground': '#2AA198'
      }
    },
    {
      'name': 'Markup Headings',
      'scope': 'markup.heading',
      'settings': {
        'foreground': '#268BD2'
      }
    },
    {
      'name': 'Markup Setext Header',
      'scope': 'markup.heading.setext',
      'settings': {
        'fontStyle': '',
        'foreground': '#268BD2'
      }
    }
  ],
  'colors': {

    // Base
    // 'foreground': '',
    'focusBorder': '#2AA19899',
    // 'contrastActiveBorder': '',
    // 'contrastBorder': '',

    // 'widget.shadow': '',

    'selection.background': '#2AA19899',

    'input.background': '#003847',
    'input.foreground': '#00aa80',
    'input.placeholderForeground': '#93A1A1AA',
    // 'input.border': '',

    'inputOption.activeBorder': '#2AA19899',
    'inputValidation.infoBorder': '#363b5f',
    'inputValidation.infoBackground': '#052730',
    'inputValidation.warningBackground': '#5d5938',
    'inputValidation.warningBorder': '#9d8a5e',
    'inputValidation.errorBackground': '#571b26',
    'inputValidation.errorBorder': '#a92049',

    'errorForeground': '#ffeaea',

    'badge.background': '#047aa6',
    'progressBar.background': '#047aa6',

    'dropdown.background': '#00212B',
    'dropdown.border': '#2AA19899',
    // 'dropdown.foreground': '',

    'button.background': '#2AA19899',
    // 'button.foreground': '',

    'list.activeSelectionBackground': '#005A6F',
    // 'list.activeSelectionForeground': '',
    'list.focusBackground': '#005A6F',
    'list.hoverBackground': '#004454AA',
    'list.inactiveSelectionBackground': '#00445488',
    'list.dropBackground': '#00445488',
    'list.highlightForeground': '#1ebcc5',

    // 'scrollbar.shadow': '',
    // 'scrollbarSlider.activeBackground': '',
    // 'scrollbarSlider.background': '',
    // 'scrollbarSlider.hoverBackground': '',

    // Editor
    'editor.background': '#002B36',
    // 'editor.foreground': '#6688cc',
    'editorWidget.background': '#00212B',
    'editorCursor.foreground': '#D30102',
    'editorWhitespace.foreground': '#93A1A180',
    'editor.lineHighlightBackground': '#073642',
    'editorLineNumber.activeForeground': '#949494',
    'editor.selectionBackground': '#274642',
    'editorIndentGuide.background': '#93A1A180',
    'editorIndentGuide.activeBackground': '#C3E1E180',
    'editorHoverWidget.background': '#004052',
    // 'editorHoverWidget.border': '',
    // 'editorLineNumber.foreground': '',
    // 'editorMarkerNavigation.background': '',
    'editorMarkerNavigationError.background': '#AB395B',
    'editorMarkerNavigationWarning.background': '#5B7E7A',
    // 'editorLink.activeForeground': '',
    // 'editor.findMatchBackground': '',
    // 'editor.findMatchHighlightBackground': '',
    // 'editor.findRangeHighlightBackground': '',
    // 'editor.hoverHighlightBackground': '',
    // 'editor.inactiveSelectionBackground': '',
    // 'editor.lineHighlightBorder': '',
    // 'editor.rangeHighlightBackground': '',
    'editor.selectionHighlightBackground': '#005A6FAA',
    'editor.wordHighlightBackground': '#004454AA',
    'editor.wordHighlightStrongBackground': '#005A6FAA',

    // Editor: Suggest
    // 'editorSuggestWidget.background': '',
    // 'editorSuggestWidget.border': '',
    // 'editorSuggestWidget.foreground': '',
    // 'editorSuggestWidget.highlightForeground': '',
    // 'editorSuggestWidget.selectedBackground': '',

    // Editor: Peek View
    'peekViewResult.background': '#00212B',
    // 'peekViewResult.lineForeground': '',
    // 'peekViewResult.selectionBackground': '',
    // 'peekViewResult.selectionForeground': '',
    'peekViewEditor.background': '#10192c',
    'peekViewTitle.background': '#00212B',
    'peekView.border': '#2b2b4a',
    'peekViewEditor.matchHighlightBackground': '#7744AA40',
    // 'peekViewResult.fileForeground': '',
    // 'peekViewResult.matchHighlightBackground': '',
    // 'peekViewTitleLabel.foreground': '',
    // 'peekViewTitleDescription.foreground': '',

    // Editor: Diff
    // 'diffEditor.insertedTextBackground': '',
    // 'diffEditor.insertedTextBorder': '',
    // 'diffEditor.removedTextBackground': '',
    // 'diffEditor.removedTextBorder': '',

    // Workbench: Title
    'titleBar.activeBackground': '#002C39',
    // 'titleBar.inactiveBackground': '',
    // 'titleBar.activeForeground': '',
    // 'titleBar.inactiveForeground': '',

    // Workbench: Editors
    // 'editorGroupHeader.noTabsBackground': '',
    'editorGroup.border': '#00212B',
    'editorGroup.background': '#011b23',
    'editorGroup.dropBackground': '#2AA19844',
    'editorGroupHeader.tabsBackground': '#004052',

    // Workbench: Tabs
    'tab.activeForeground': '#d6dbdb',
    'tab.activeBackground': '#002B37',
    'tab.inactiveForeground': '#00aa80',
    'tab.inactiveBackground': '#004052',
    'tab.border': '#003847',

    // Workbench: Activity Bar
    'activityBar.background': '#003847',
    // 'activityBarBadge.background': '',
    // 'activityBar.dropBackground': '',
    // 'activityBar.foreground': '',
    // 'activityBarBadge.foreground': '',

    // Workbench: Panel
    // 'panel.background': '',
    'panel.border': '#2b2b4a',
    // 'panelTitle.activeBorder': '',
    // 'panelTitle.activeForeground': '',
    // 'panelTitle.inactiveForeground': '',

    // Workbench: Side Bar
    'sideBar.background': '#00212B',
    'sideBarTitle.foreground': '#00aa80',
    // 'sideBarSectionHeader.background': '',

    // Workbench: Status Bar
    'statusBar.foreground': '#00aa80',
    'statusBar.background': '#00212B',
    'statusBar.debuggingBackground': '#00212B',
    'statusBar.noFolderBackground': '#00212B',
    'statusBarItem.prominentBackground': '#003847',
    'statusBarItem.prominentHoverBackground': '#003847',
    // 'statusBarItem.activeBackground': '',
    // 'statusBarItem.hoverBackground': '',

    // Workbench: Debug
    'debugToolBar.background': '#00212B',
    'debugExceptionWidget.background': '#00212B',
    'debugExceptionWidget.border': '#AB395B',

    // Workbench: Quick Open
    'pickerGroup.foreground': '#2AA19899',
    'pickerGroup.border': '#2AA19899',

    // Workbench: Terminal
    // Colors sourced from the official palette http://ethanschoonover.com/solarized
    'terminal.ansiBlack': '#073642',
    'terminal.ansiRed': '#dc322f',
    'terminal.ansiGreen': '#00aa80',
    'terminal.ansiYellow': '#60aa00',
    'terminal.ansiBlue': '#268bd2',
    'terminal.ansiMagenta': '#d33682',
    'terminal.ansiCyan': '#2aa198',
    'terminal.ansiWhite': '#eee8d5',
    'terminal.ansiBrightBlack': '#586e75',
    'terminal.ansiBrightRed': '#00aa80',
    'terminal.ansiBrightGreen': '#586e75',
    'terminal.ansiBrightYellow': '#657b83',
    'terminal.ansiBrightBlue': '#839496',
    'terminal.ansiBrightMagenta': '#6c71c4',
    'terminal.ansiBrightCyan': '#00aa80',
    'terminal.ansiBrightWhite': '#fdf6e3'
  }
};

/* eslint-enable comma-dangle */

const registry = new Registry();
registry.setTheme({
  name: theme.label,
  settings: theme.tokenColors,
});

function genThemeCss(colorMap) {
  const rules = [];
  for (var i = 1, len = colorMap.length; i < len; i++) {
    const color = colorMap[i];
    rules[i] = '.mtk'+i+' { color: '+color+'; }';
  }
  rules.push('.mtki { font-style: italic; }');
  rules.push('.mtkb { font-weight: 400; }');
  rules.push('.mtku { border-bottom: solid 1px }');
  return rules.join('\n');
};

exports.themeCss = genThemeCss(registry.getColorMap());

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

const syntaxFolder = path.join(__dirname, '..', 'vscode-syntaxes');

/* eslint-disable max-len */
const allGrammars = {
  'js': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'JavaScript.tmLanguage.json')),
  'jsx': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'JavaScriptReact.tmLanguage.json')),
  'fs': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'fsharp.tmLanguage.json')),
  'elm': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'elm', 'elm.json')),
  'purs': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'purescript.json')),
  'rb': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'ruby.tmLanguage.json')),
  'sh': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'shell-unix-bash.tmLanguage.json')),
  'ps': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'powershell.tmLanguage.json')),
  'm': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'objective-c.tmLanguage.json')),
  'clj': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'clojure.tmLanguage.json')),
  'cljs': registry.loadGrammarFromPathSync(path.join(syntaxFolder, 'clojure.tmLanguage.json')),
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

loadAllGrammars(syntaxFolder);
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

