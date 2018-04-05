// import List, Native.Utils, Native.List, Native.Debug //


var _chendesheng$webvim_elm$Native_Highlight = function() {
  function addClass(classnames, classname) {
    if (classname === '') return classnames
    else if (classnames === '') return classname;
    else classnames + ' ' + classname;
  }

  function parseResultHelper(node, path, result) {
    var len = node.childNodes.length;
    for (var i = 0; i < len; i++) {
      var child = node.childNodes[i];
      if (child.nodeType === 3) {
        result = _elm_lang$core$Native_List.Cons(
          _elm_lang$core$Native_Utils.Tuple2(path, child.textContent),
          result
        );
      } else if (child.nodeType === 1) {
        result = parseResultHelper(
          child,
          addClass(path, child.className),
          result
        );
      } else {
        throw 'unexpect child: ' + child.textContent;
      }
    }
    return result;
  }

  function parseResult(html) {
    var container = document.createElement('div');
    container.innerHTML = html;
    return _elm_lang$core$List$reverse(parseResultHelper(container,
      '',
      _elm_lang$core$Native_List.Nil
    ));
  }

  function highlight(lang, line, continuation) {
    var top = null;
    if (continuation.ctor === 'Just') {
      top = continuation._0._0;
    }

    var result = hljs.highlight(lang, line, false, top);
    var scopes = parseResult(result.value);

    // console.log(_elm_lang$core$Native_Utils.toString(scopes));
    
    return _elm_lang$core$Native_Utils.Tuple2(
      scopes,
      { ctor: '<decoder>', // top is opaque
        _0: result.top
      }
    );
  }

  if (typeof hljs == 'undefined') {
    // make elm repl working
    return {
      highlight: F3(function(a,b,c){
        return _elm_lang$core$Native_Utils.Tuple2(
          _elm_lang$core$Native_List.Nil,
          {}
        );
      }),
    };
  } else {
    return {
      highlight: F3(highlight),
    };
  }
}();

