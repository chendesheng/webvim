/*

import Elm.Kernel.Utils exposing(Tuple2)
import Elm.Kernel.List exposing(fromArray, Cons, Nil)
import Maybe exposing (Just, Nothing)

*/


// PARSER

var _TreeSitter_dummyParser = {
  $: 'treeSitter_parser',
  __parser: {
    parse: function() {
      return {
        $: 'treeSitter_tree',
        __tree: {
          rootNode: {
            childCount: 0,
            children: [],
            startIndex: 0,
            endIndex: 0,
            startPosition: { row: 0, column: 0 },
            endPosition: { row:0, column: 0 },
          }
        }
      };
    }
  }
};

var _TreeSitter_createParser = function (name) {
  var lang = window.treeSitterLangs[name];
  if (lang) {
    var parser = new window.TreeSitter;
    parser.setLanguage(window.treeSitterLangs[name]);
    return __Maybe_Just({
      $: 'treeSitter_parser',
      __parser: parser,
    });
  } else {
    return __Maybe_Nothing;
  }
};

function parseHelper(parser, input, text, tree) {
  var currentText = text;
  return parser.__parser.parse(function (startIndex, startPosition, endIndex) {
		var res = A2(input, currentText, {
			startIndex,
			startPosition: startPosition ? __Maybe_Just(toTuple(startPosition)) : __Maybe_Nothing,
			endIndex: endIndex != null ? __Maybe_Just(endIndex) : __Maybe_Nothing,
		});
    currentText = res.b;
    return res.a;
  }, tree);
}

var _TreeSitter_parse = F2(function (parser, input, text) {
  return { $: 'treeSitter_tree', __tree: parseHelper(parser, input, text) };
});

function toTuple({ row, column }) {
	return __Utils_Tuple2(row, column);
}

function fromTuple(tp) {
  return { row : tp.a, col : tp.b };
}

var _TreeSitter_incParse = F4(function (parser, edits, input, text, oldTreeObj) {
  var oldTree = oldTreeObj.__tree;
  for (var item = edits; item.b; item = item.b) // WHILE_CONS
  {
    var edit = item.a;
    oldTree.edit(Object.assign(edit, {
      startPoisition : fromTuple(edit.startPosition), 
      newEndPoisition : fromTuple(edit.newEndPosition), 
      oldEndPoisition : fromTuple(edit.oldEndPosition), 
    }));
  }

  var newTree = parseHelper(parser, input, text, oldTree);
	var ranges =
		newTree
			.getChangdRanges(oldTree)
			.map(function (rg) {
				return Object.assign(rg, {
					startPosition: toTuple(rg.startPoisition),
					endPosition: toTuple(rg.endPoisition),
				})
			});
	oldTree.delete(); // This is not pure but I don't how to avoid
	return __Utils_Tuple2({$:'treeSitter_tree', __tree: newTree}, __List_fromArray(ranges));
});

// TREE NODE

var _TreeSitter_root = function (tree) {
	return tree.__tree.rootNode;
};

var _TreeSitter_childCount = function (node) {
	return node.childCount;
};

var _TreeSitter_child = F2(function (node, i) {
 	if (i < node.childCount) {
 		return __Maybe_Just(node.child(i));
 	}
 	return __Maybe_Nothing;
});

var _TreeSitter_children = function (node) {
  return __List_fromArray(node.children);
};

var _TreeSitter_getScope = function (node) {
	return {
		scope: node.type,
		startIndex: node.startIndex,
		endIndex: node.endIndex,
		startPosition: toTuple(node.startPosition),
		endPosition: toTuple(node.endPosition),
	};
};

function cursorToScope(cursor) {
	return {
		scope: cursor.nodeType,
		startIndex: cursor.startIndex,
		endIndex: cursor.endIndex,
		startPosition: toTuple(cursor.startPosition),
		endPosition: toTuple(cursor.endPosition),
	};
}

function comparePosition(y1, x1, y2, x2) {
  const d = y1 - y2;
  return d === 0 ? x1 - x2 : d;
}

var _TreeSitter_walkWithin = F5(function(start, end, reducer, tree, state) {
  const cursor = tree.__tree.walk();
  let visitedChildren = false;

  let ancests = __List_Nil;

  while (true) {
    if (visitedChildren) {
      ancests = ancests.b;

      if (cursor.gotoNextSibling()) {
        visitedChildren = false;
      } else if (cursor.gotoParent()) {
        visitedChildren = true;
      } else {
        break;
      }
    } else {
      const s = cursor.startPosition;
      const e = cursor.endPosition;

      if (comparePosition(s.row, s.column, end.a, end.b) >= 0) {
        // skip forward nodes
        break;
      }

      ancests = __List_Cons(cursorToScope(cursor), ancests);

      // skip subtree when pos less than start 
      if (comparePosition(e.row, e.column, start.a, start.b) < 0) {
        visitedChildren = true;
        continue;
      }

      if (cursor.gotoFirstChild()) {
        visitedChildren = false;
      } else {
        state = A2(reducer, ancests, state);
        visitedChildren = true;
      }
    }
  }

  cursor.delete();
  return state;
});


