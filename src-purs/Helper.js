/* eslint-disable no-var */
var streams = require('memory-streams');
exports.readAllString = function(readable) {
  return function(callback) {
    return function() {
      var w = new streams.WritableStream();
      readable.pipe(w);
      readable.on('readable', function() {
        callback(w.toString())();
      });
    };
  };
};
