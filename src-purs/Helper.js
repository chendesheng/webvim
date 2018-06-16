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
  return new MemoryStream(s, {writable: false});
};
