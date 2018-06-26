module.exports = function(results) {
  // the source field is quite big and useless (for now), just remove it
  for (let i = 0, len = results.length; i < len; i++) {
    const result = results[i];
    result.source = undefined;
    result.errorCount = undefined;
    result.warningCount = undefined;
    result.fixableErrorCount = undefined;
    result.fixableWarningCount = undefined;
  }

  return JSON.stringify(results);
};
