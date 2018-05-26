//check https://github.com/google/closure-compiler/issues/1875
var JSCOMPILER_PRESERVE = function() {}; 
var _chendesheng$webvim_elm$Native_Doc = function() {

var fakeNode = {
  getElementById: function() { return null; },
  addEventListener: function() {},
  removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);

function on(node)
{
  return function(eventName, decoder, toTask)
  {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

        function performTask(event)
        {
          event.preventDefault(); // hack

          var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
          if (result.ctor === 'Ok')
          {
              _elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
          }
        }

        node.addEventListener(eventName, performTask);

        return function()
        {
          node.removeEventListener(eventName, performTask);
        };
    });
  };
}

// copy from https://github.com/hiddentao/fast-levenshtein
// arrays to re-use
var prevRow = [],
  str2Char = [];
  
/**
 * Based on the algorithm at http://en.wikipedia.org/wiki/Levenshtein_distance.
 */
function levenshtein(str1, str2) {
  
  var str1Len = str1.length,
    str2Len = str2.length;
  
  // base cases
  if (str1Len === 0) return str2Len;
  if (str2Len === 0) return str1Len;

  // two rows
  var curCol, nextCol, i, j, tmp;

  // initialise previous row
  for (i=0; i<str2Len; ++i) {
    prevRow[i] = i;
    str2Char[i] = str2.charCodeAt(i);
  }
  prevRow[str2Len] = str2Len;

  var strCmp;
  // calculate current row distance from previous row without collator
  for (i = 0; i < str1Len; ++i) {
    nextCol = i + 1;

    for (j = 0; j < str2Len; ++j) {
      curCol = nextCol;

      // substution
      strCmp = str1.charCodeAt(i) === str2Char[j];

      nextCol = prevRow[j] + (strCmp ? 0 : 1);

      // insertion
      tmp = curCol + 1;
      if (nextCol > tmp) {
        nextCol = tmp;
      }
      // deletion
      tmp = prevRow[j + 1] + 1;
      if (nextCol > tmp) {
        nextCol = tmp;
      }

      // copy current col value into previous (in preparation for next iteration)
      prevRow[j] = curCol;
    }

    // copy last col value into previous (in preparation for next iteration)
    prevRow[j] = nextCol;
  }
  return nextCol;
}



return {
  onDocument: F3(onDocument),
  checkRegex: function(s) {
    if (!s) return false;

    try {
      new RegExp(s);
      return true;
    } catch(e) {
      return false;
    }
  },
  levenshtein: F2(levenshtein)
};

}();

