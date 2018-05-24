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
};

}();

