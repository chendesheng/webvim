// Native.Utils //

var _chendesheng$webvim_elm$Native_JsArray = function() {

  var _JsArray_empty = [];

  function _JsArray_singleton(value)
  {
      return [value];
  }

  function _JsArray_length(array)
  {
      return array.length;
  }

  var _JsArray_initialize = F3(function(size, offset, func)
  {
      var result = new Array(size);

      for (var i = 0; i < size; i++)
      {
          result[i] = func(offset + i);
      }

      return result;
  });

  var _JsArray_initializeFromList = F2(function (max, ls)
  {
      var result = new Array(max);

      for (var i = 0; i < max && ls._1; i++)
      {
          result[i] = ls._0;
          ls = ls._1;
      }

      result.length = i;
      return _elm_lang$core$Native_Utils.Tuple2(result, ls);
  });

  var _JsArray_unsafeGet = F2(function(index, array)
  {
      return array[index];
  });

  var _JsArray_unsafeSet = F3(function(index, value, array)
  {
      var length = array.length;
      var result = new Array(length);

      for (var i = 0; i < length; i++)
      {
          result[i] = array[i];
      }

      result[index] = value;
      return result;
  });

  var _JsArray_push = F2(function(value, array)
  {
      var length = array.length;
      var result = new Array(length + 1);

      for (var i = 0; i < length; i++)
      {
          result[i] = array[i];
      }

      result[length] = value;
      return result;
  });

  var _JsArray_foldl = F3(function(func, acc, array)
  {
      var length = array.length;

      for (var i = 0; i < length; i++)
      {
          acc = A2(func, array[i], acc);
      }

      return acc;
  });

  var _JsArray_foldr = F3(function(func, acc, array)
  {
      for (var i = array.length - 1; i >= 0; i--)
      {
          acc = A2(func, array[i], acc);
      }

      return acc;
  });

  var _JsArray_map = F2(function(func, array)
  {
      var length = array.length;
      var result = new Array(length);

      for (var i = 0; i < length; i++)
      {
          result[i] = func(array[i]);
      }

      return result;
  });

  var _JsArray_indexedMap = F3(function(func, offset, array)
  {
      var length = array.length;
      var result = new Array(length);

      for (var i = 0; i < length; i++)
      {
          result[i] = A2(func, offset + i, array[i]);
      }

      return result;
  });

  var _JsArray_slice = F3(function(from, to, array)
  {
      return array.slice(from, to);
  });

  var _JsArray_appendN = F3(function(n, dest, source)
  {
      var destLen = dest.length;
      var itemsToCopy = n - destLen;

      if (itemsToCopy > source.length)
      {
          itemsToCopy = source.length;
      }

      var size = destLen + itemsToCopy;
      var result = new Array(size);

      for (var i = 0; i < destLen; i++)
      {
          result[i] = dest[i];
      }

      for (var i = 0; i < itemsToCopy; i++)
      {
          result[i + destLen] = source[i];
      }

      return result;
  });

  return {
    empty: _JsArray_empty,
    singleton : _JsArray_singleton,
    length: _JsArray_length,
    initialize: _JsArray_initialize,
    initializeFromList: _JsArray_initializeFromList,
    unsafeGet: _JsArray_unsafeGet,
    unsafeSet: _JsArray_unsafeSet,
    push: _JsArray_push,
    foldl: _JsArray_foldl,
    foldr: _JsArray_foldr,
    map: _JsArray_map,
    indexedMap: _JsArray_indexedMap,
    slice: _JsArray_slice,
    appendN: _JsArray_appendN,
  };
}();
