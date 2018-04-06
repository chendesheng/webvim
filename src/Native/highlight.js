// import List, Native.Utils, Native.List, Native.Debug //


var _chendesheng$webvim_elm$Native_Highlight = function() {
  function htmlDecode(s) {
    return s.replace(/&(amp|gt|lt);/g, function(cap){
      if (cap=='&gt;') return '>';
      else if (cap == '&lt;') return '<';
      else return '&';
    });
  }

  function addScopes(scopes, classnames, text) {
    if (text == '') return scopes;
    scopes.push(_elm_lang$core$Native_Utils.Tuple2(classnames,
      htmlDecode(text)));
    return scopes;
  }

  // returns [[scopes, restString]]
  function spanParser(s, path) {
    var prefix = '<span class="';
    s = s.substring(prefix.length);
    var scopes = [];
    var i = s.indexOf('">');
    if (i == -1) {
      scopes = addScopes(scopes, path, s);
      return [[scopes, '']];
    }
    var classname = s.substring(0, i);
    s = s.substring(i+2);
    path = addClass(path, classname);

    while (true) {
      i = s.search(/<span|<\/span>/)

      // not found
      if (i == -1) {
        scopes = addScopes(scopes, path, s);
        return [[scopes, '']];
      }

      var text = s.substring(0, i);
      s = s.substring(i);
      scopes = addScopes(scopes, path, text);

      // find close tag
      if (s.substring(0, 2) == '</') {
        return [[scopes, s.substring('</span>'.length)]];
      }

      // find sub span
      var result = spanParser(s, path)
      if (result.length == 0) {
        return [];
      }
      scopes = scopes.concat(result[0][0]);
      s = result[0][1];
    }
  }

  function addClass(classnames, classname) {
    if (classname === '') return classnames
    else if (classnames === '') return classname;
    else return classnames + ' ' + classname;
  }

  function parseResult(html) {
    var result = spanParser('<span class="">'+html+'</span>', '');
    var scopes = result[0][0];
    var list = _elm_lang$core$Native_List.Nil;
    for (var i = scopes.length - 1; i >= 0; i--) {
      list = _elm_lang$core$Native_List.Cons(scopes[i], list);
    }
    return list;
  }

  function highlight(lang, line, continuation) {
    var top = null;
    if (continuation.ctor === 'Just') {
      top = continuation._0._0;
    }

    var result = hljs.highlight(lang, line, false, top);
    var scopes = parseResult(result.value);

    // console.log(_elm_lang$core$Native_Utils.toString(scopes));
    // console.log(top);
    
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

