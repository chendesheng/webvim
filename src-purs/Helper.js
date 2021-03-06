/* eslint-disable no-var */
var MemoryStream = require('memorystream');

// the readable must trigger the 'end' event
exports.readAllString = function(readable) {
  return function(callback) {
    return function() {
      var s = '';
      readable.on('data', function(chunk) {
        s += chunk;
      });
      readable.on('end', function() {
        callback(s)();
      });
    };
  };
};

exports.createReadableStream = function(s) {
  return new MemoryStream(s, {
    writable: false,
  });
};

var jsdiff = require('diff');
// return patch from old to new
exports.diff = function(oldStr) {
  return function(newStr) {
    // console.log(oldStr);
    // console.log('newStr: ' + newStr);
    var changes = jsdiff.diffLines(
      oldStr, newStr
    );
    // console.log(changes.map(function(change) {
    //   return {
    //     count: change.count,
    //     added: change.added,
    //     removed: change.removed,
    //   };
    // }));
    var result = [];
    var line = 0;
    for (var i = 0; i < changes.length; i++) {
      var change = changes[i];
      if (change.added) {
        result.push({
          type: '+',
          from: line,
          value: change.value,
        });
        line += change.count;
      } else if (change.removed) {
        result.push({
          type: '-',
          from: line,
          to: line + change.count,
        });
      } else {
        line += change.count;
      }
    }
    return JSON.stringify(result);
  };
};

exports.homedir = require('os').homedir();

exports.currentdir = __dirname;

var os = require('os');
exports.tempdir = os.tmpdir();

exports.boot = JSON.stringify({
  homedir: exports.homedir,
  pathSeperator: require('path').sep,
});

exports.isWindows = /^win/i.test(require('os').platform());

var clipboardy = require('clipboardy');
exports.readClipboard = clipboardy.readSync;
exports.writeClipboard = function(s) {
  return function() {
    console.log(s);
    clipboardy.writeSync(s);
  };
};

exports.argv = process.argv;

var mime = require('mime-types');
exports.getMime = function(name) {
  return mime.lookup(name);
};
