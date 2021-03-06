// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/bundle-url.js":[function(require,module,exports) {
var bundleURL = null;

function getBundleURLCached() {
  if (!bundleURL) {
    bundleURL = getBundleURL();
  }

  return bundleURL;
}

function getBundleURL() {
  // Attempt to find the URL of the current script and use that as the base URL
  try {
    throw new Error();
  } catch (err) {
    var matches = ('' + err.stack).match(/(https?|file|ftp|chrome-extension|moz-extension):\/\/[^)\n]+/g);

    if (matches) {
      return getBaseURL(matches[0]);
    }
  }

  return '/';
}

function getBaseURL(url) {
  return ('' + url).replace(/^((?:https?|file|ftp|chrome-extension|moz-extension):\/\/.+)\/[^/]+$/, '$1') + '/';
}

exports.getBundleURL = getBundleURLCached;
exports.getBaseURL = getBaseURL;
},{}],"C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/css-loader.js":[function(require,module,exports) {
var bundle = require('./bundle-url');

function updateLink(link) {
  var newLink = link.cloneNode();

  newLink.onload = function () {
    link.remove();
  };

  newLink.href = link.href.split('?')[0] + '?' + Date.now();
  link.parentNode.insertBefore(newLink, link.nextSibling);
}

var cssTimeout = null;

function reloadCSS() {
  if (cssTimeout) {
    return;
  }

  cssTimeout = setTimeout(function () {
    var links = document.querySelectorAll('link[rel="stylesheet"]');

    for (var i = 0; i < links.length; i++) {
      if (bundle.getBaseURL(links[i].href) === bundle.getBundleURL()) {
        updateLink(links[i]);
      }
    }

    cssTimeout = null;
  }, 50);
}

module.exports = reloadCSS;
},{"./bundle-url":"C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/bundle-url.js"}],"src/style.css":[function(require,module,exports) {
var reloadCSS = require('_css_loader');

module.hot.dispose(reloadCSS);
module.hot.accept(reloadCSS);
},{"_css_loader":"C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/css-loader.js"}],"src/Main.elm":[function(require,module,exports) {
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEBUG mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



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

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
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



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_enqueueEffects(managers, result.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// HELPERS


function _Debugger_unsafeCoerce(value)
{
	return value;
}



// PROGRAMS


var _Debugger_element = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init),
		$elm$browser$Debugger$Main$wrapUpdate(impl.update),
		$elm$browser$Debugger$Main$wrapSubs(impl.subscriptions),
		function(sendToApp, initialModel)
		{
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			var currNode = _VirtualDom_virtualize(domNode);
			var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
			var currPopout;

			var cornerNode = _VirtualDom_doc.createElement('div');
			domNode.parentNode.insertBefore(cornerNode, domNode.nextSibling);
			var cornerCurr = _VirtualDom_virtualize(cornerNode);

			initialModel.popout.a = sendToApp;

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = A2(_VirtualDom_map, $elm$browser$Debugger$Main$UserMsg, view($elm$browser$Debugger$Main$getUserModel(model)));
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;

				// update blocker

				var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
				_Debugger_updateBlocker(currBlocker, nextBlocker);
				currBlocker = nextBlocker;

				// view corner

				var cornerNext = $elm$browser$Debugger$Main$cornerView(model);
				var cornerPatches = _VirtualDom_diff(cornerCurr, cornerNext);
				cornerNode = _VirtualDom_applyPatches(cornerNode, cornerCurr, cornerPatches, sendToApp);
				cornerCurr = cornerNext;

				if (!model.popout.b)
				{
					currPopout = undefined;
					return;
				}

				// view popout

				_VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
				currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
				var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
				var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
				_VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
				currPopout = nextPopout;
				_VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
			});
		}
	);
});


var _Debugger_document = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init),
		$elm$browser$Debugger$Main$wrapUpdate(impl.update),
		$elm$browser$Debugger$Main$wrapSubs(impl.subscriptions),
		function(sendToApp, initialModel)
		{
			var divertHrefToApp = impl.setup && impl.setup(function(x) { return sendToApp($elm$browser$Debugger$Main$UserMsg(x)); });
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
			var currPopout;

			initialModel.popout.a = sendToApp;

			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view($elm$browser$Debugger$Main$getUserModel(model));
				var nextNode = _VirtualDom_node('body')(_List_Nil)(
					_Utils_ap(
						A2($elm$core$List$map, _VirtualDom_map($elm$browser$Debugger$Main$UserMsg), doc.body),
						_List_Cons($elm$browser$Debugger$Main$cornerView(model), _List_Nil)
					)
				);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);

				// update blocker

				var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
				_Debugger_updateBlocker(currBlocker, nextBlocker);
				currBlocker = nextBlocker;

				// view popout

				if (!model.popout.b) { currPopout = undefined; return; }

				_VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
				currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
				var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
				var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
				_VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
				currPopout = nextPopout;
				_VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
			});
		}
	);
});


function _Debugger_popout()
{
	return {
		b: undefined,
		a: undefined
	};
}

function _Debugger_isOpen(popout)
{
	return !!popout.b;
}

function _Debugger_open(popout)
{
	return _Scheduler_binding(function(callback)
	{
		_Debugger_openWindow(popout);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}

function _Debugger_openWindow(popout)
{
	var w = $elm$browser$Debugger$Main$initialWindowWidth,
		h = $elm$browser$Debugger$Main$initialWindowHeight,
	 	x = screen.width - w,
		y = screen.height - h;

	var debuggerWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);
	var doc = debuggerWindow.document;
	doc.title = 'Elm Debugger';

	// handle arrow keys
	doc.addEventListener('keydown', function(event) {
		event.metaKey && event.which === 82 && window.location.reload();
		event.key === 'ArrowUp'   && (popout.a($elm$browser$Debugger$Main$Up  ), event.preventDefault());
		event.key === 'ArrowDown' && (popout.a($elm$browser$Debugger$Main$Down), event.preventDefault());
	});

	// handle window close
	window.addEventListener('unload', close);
	debuggerWindow.addEventListener('unload', function() {
		popout.b = undefined;
		popout.a($elm$browser$Debugger$Main$NoOp);
		window.removeEventListener('unload', close);
	});

	function close() {
		popout.b = undefined;
		popout.a($elm$browser$Debugger$Main$NoOp);
		debuggerWindow.close();
	}

	// register new window
	popout.b = doc;
}



// SCROLL


function _Debugger_scroll(popout)
{
	return _Scheduler_binding(function(callback)
	{
		if (popout.b)
		{
			var msgs = popout.b.getElementById('elm-debugger-sidebar');
			if (msgs && msgs.scrollTop !== 0)
			{
				msgs.scrollTop = 0;
			}
		}
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


var _Debugger_scrollTo = F2(function(id, popout)
{
	return _Scheduler_binding(function(callback)
	{
		if (popout.b)
		{
			var msg = popout.b.getElementById(id);
			if (msg)
			{
				msg.scrollIntoView(false);
			}
		}
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});



// UPLOAD


function _Debugger_upload(popout)
{
	return _Scheduler_binding(function(callback)
	{
		var doc = popout.b || document;
		var element = doc.createElement('input');
		element.setAttribute('type', 'file');
		element.setAttribute('accept', 'text/json');
		element.style.display = 'none';
		element.addEventListener('change', function(event)
		{
			var fileReader = new FileReader();
			fileReader.onload = function(e)
			{
				callback(_Scheduler_succeed(e.target.result));
			};
			fileReader.readAsText(event.target.files[0]);
			doc.body.removeChild(element);
		});
		doc.body.appendChild(element);
		element.click();
	});
}



// DOWNLOAD


var _Debugger_download = F2(function(historyLength, json)
{
	return _Scheduler_binding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = _Scheduler_succeed(_Utils_Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
});



// POPOUT CONTENT


function _Debugger_messageToString(value)
{
	if (typeof value === 'boolean')
	{
		return value ? 'True' : 'False';
	}

	if (typeof value === 'number')
	{
		return value + '';
	}

	if (typeof value === 'string')
	{
		return '"' + _Debugger_addSlashes(value, false) + '"';
	}

	if (value instanceof String)
	{
		return "'" + _Debugger_addSlashes(value, true) + "'";
	}

	if (typeof value !== 'object' || value === null || !('$' in value))
	{
		return '…';
	}

	if (typeof value.$ === 'number')
	{
		return '…';
	}

	var code = value.$.charCodeAt(0);
	if (code === 0x23 /* # */ || /* a */ 0x61 <= code && code <= 0x7A /* z */)
	{
		return '…';
	}

	if (['Array_elm_builtin', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.$) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.$;
		case 2:
			return value.$ + ' ' + _Debugger_messageToString(value.a);
		default:
			return value.$ + ' … ' + _Debugger_messageToString(value[keys[keys.length - 1]]);
	}
}


function _Debugger_init(value)
{
	if (typeof value === 'boolean')
	{
		return A3($elm$browser$Debugger$Expando$Constructor, $elm$core$Maybe$Just(value ? 'True' : 'False'), true, _List_Nil);
	}

	if (typeof value === 'number')
	{
		return $elm$browser$Debugger$Expando$Primitive(value + '');
	}

	if (typeof value === 'string')
	{
		return $elm$browser$Debugger$Expando$S('"' + _Debugger_addSlashes(value, false) + '"');
	}

	if (value instanceof String)
	{
		return $elm$browser$Debugger$Expando$S("'" + _Debugger_addSlashes(value, true) + "'");
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (tag === '::' || tag === '[]')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ListSeq, true,
				A2($elm$core$List$map, _Debugger_init, value)
			);
		}

		if (tag === 'Set_elm_builtin')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$SetSeq, true,
				A3($elm$core$Set$foldr, _Debugger_initCons, _List_Nil, value)
			);
		}

		if (tag === 'RBNode_elm_builtin' || tag == 'RBEmpty_elm_builtin')
		{
			return A2($elm$browser$Debugger$Expando$Dictionary, true,
				A3($elm$core$Dict$foldr, _Debugger_initKeyValueCons, _List_Nil, value)
			);
		}

		if (tag === 'Array_elm_builtin')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ArraySeq, true,
				A3($elm$core$Array$foldr, _Debugger_initCons, _List_Nil, value)
			);
		}

		if (typeof tag === 'number')
		{
			return $elm$browser$Debugger$Expando$Primitive('<internals>');
		}

		var char = tag.charCodeAt(0);
		if (char === 35 || 65 <= char && char <= 90)
		{
			var list = _List_Nil;
			for (var i in value)
			{
				if (i === '$') continue;
				list = _List_Cons(_Debugger_init(value[i]), list);
			}
			return A3($elm$browser$Debugger$Expando$Constructor, char === 35 ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(tag), true, $elm$core$List$reverse(list));
		}

		return $elm$browser$Debugger$Expando$Primitive('<internals>');
	}

	if (typeof value === 'object')
	{
		var dict = $elm$core$Dict$empty;
		for (var i in value)
		{
			dict = A3($elm$core$Dict$insert, i, _Debugger_init(value[i]), dict);
		}
		return A2($elm$browser$Debugger$Expando$Record, true, dict);
	}

	return $elm$browser$Debugger$Expando$Primitive('<internals>');
}

var _Debugger_initCons = F2(function initConsHelp(value, list)
{
	return _List_Cons(_Debugger_init(value), list);
});

var _Debugger_initKeyValueCons = F3(function(key, value, list)
{
	return _List_Cons(
		_Utils_Tuple2(_Debugger_init(key), _Debugger_init(value)),
		list
	);
});

function _Debugger_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}



// BLOCK EVENTS


function _Debugger_updateBlocker(oldBlocker, newBlocker)
{
	if (oldBlocker === newBlocker) return;

	var oldEvents = _Debugger_blockerToEvents(oldBlocker);
	var newEvents = _Debugger_blockerToEvents(newBlocker);

	// remove old blockers
	for (var i = 0; i < oldEvents.length; i++)
	{
		document.removeEventListener(oldEvents[i], _Debugger_blocker, true);
	}

	// add new blockers
	for (var i = 0; i < newEvents.length; i++)
	{
		document.addEventListener(newEvents[i], _Debugger_blocker, true);
	}
}


function _Debugger_blocker(event)
{
	if (event.type === 'keydown' && event.metaKey && event.which === 82)
	{
		return;
	}

	var isScroll = event.type === 'scroll' || event.type === 'wheel';
	for (var node = event.target; node; node = node.parentNode)
	{
		if (isScroll ? node.id === 'elm-debugger-details' : node.id === 'elm-debugger-overlay')
		{
			return;
		}
	}

	event.stopPropagation();
	event.preventDefault();
}

function _Debugger_blockerToEvents(blocker)
{
	return blocker === $elm$browser$Debugger$Overlay$BlockNone
		? []
		: blocker === $elm$browser$Debugger$Overlay$BlockMost
			? _Debugger_mostEvents
			: _Debugger_allEvents;
}

var _Debugger_mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var _Debugger_allEvents = _Debugger_mostEvents.concat('wheel', 'scroll');




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };
key['elm-hot-nav-key'] = true

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Debugger$Expando$ArraySeq = {$: 'ArraySeq'};
var $elm$browser$Debugger$Overlay$BlockMost = {$: 'BlockMost'};
var $elm$browser$Debugger$Overlay$BlockNone = {$: 'BlockNone'};
var $elm$browser$Debugger$Expando$Constructor = F3(
	function (a, b, c) {
		return {$: 'Constructor', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$Dictionary = F2(
	function (a, b) {
		return {$: 'Dictionary', a: a, b: b};
	});
var $elm$browser$Debugger$Main$Down = {$: 'Down'};
var $elm$browser$Debugger$Expando$ListSeq = {$: 'ListSeq'};
var $elm$browser$Debugger$Main$NoOp = {$: 'NoOp'};
var $elm$browser$Debugger$Expando$Primitive = function (a) {
	return {$: 'Primitive', a: a};
};
var $elm$browser$Debugger$Expando$Record = F2(
	function (a, b) {
		return {$: 'Record', a: a, b: b};
	});
var $elm$browser$Debugger$Expando$S = function (a) {
	return {$: 'S', a: a};
};
var $elm$browser$Debugger$Expando$Sequence = F3(
	function (a, b, c) {
		return {$: 'Sequence', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$SetSeq = {$: 'SetSeq'};
var $elm$browser$Debugger$Main$Up = {$: 'Up'};
var $elm$browser$Debugger$Main$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var $elm$browser$Debugger$Main$Export = {$: 'Export'};
var $elm$browser$Debugger$Main$Import = {$: 'Import'};
var $elm$browser$Debugger$Main$Open = {$: 'Open'};
var $elm$browser$Debugger$Main$OverlayMsg = function (a) {
	return {$: 'OverlayMsg', a: a};
};
var $elm$browser$Debugger$Main$Resume = {$: 'Resume'};
var $elm$browser$Debugger$Main$isPaused = function (state) {
	if (state.$ === 'Running') {
		return false;
	} else {
		return true;
	}
};
var $elm$browser$Debugger$History$size = function (history) {
	return history.numMessages;
};
var $elm$browser$Debugger$Overlay$Accept = function (a) {
	return {$: 'Accept', a: a};
};
var $elm$browser$Debugger$Overlay$Choose = F2(
	function (a, b) {
		return {$: 'Choose', a: a, b: b};
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$browser$Debugger$Overlay$goodNews1 = '\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n';
var $elm$browser$Debugger$Overlay$goodNews2 = '\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n';
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $elm$html$Html$code = _VirtualDom_node('code');
var $elm$browser$Debugger$Overlay$viewCode = function (name) {
	return A2(
		$elm$html$Html$code,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(name)
			]));
};
var $elm$browser$Debugger$Overlay$addCommas = function (items) {
	if (!items.b) {
		return '';
	} else {
		if (!items.b.b) {
			var item = items.a;
			return item;
		} else {
			if (!items.b.b.b) {
				var item1 = items.a;
				var _v1 = items.b;
				var item2 = _v1.a;
				return item1 + (' and ' + item2);
			} else {
				var lastItem = items.a;
				var otherItems = items.b;
				return A2(
					$elm$core$String$join,
					', ',
					_Utils_ap(
						otherItems,
						_List_fromArray(
							[' and ' + lastItem])));
			}
		}
	}
};
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$browser$Debugger$Overlay$problemToString = function (problem) {
	switch (problem.$) {
		case 'Function':
			return 'functions';
		case 'Decoder':
			return 'JSON decoders';
		case 'Task':
			return 'tasks';
		case 'Process':
			return 'processes';
		case 'Socket':
			return 'web sockets';
		case 'Request':
			return 'HTTP requests';
		case 'Program':
			return 'programs';
		default:
			return 'virtual DOM values';
	}
};
var $elm$browser$Debugger$Overlay$viewProblemType = function (_v0) {
	var name = _v0.name;
	var problems = _v0.problems;
	return A2(
		$elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				$elm$browser$Debugger$Overlay$viewCode(name),
				$elm$html$Html$text(
				' can contain ' + ($elm$browser$Debugger$Overlay$addCommas(
					A2($elm$core$List$map, $elm$browser$Debugger$Overlay$problemToString, problems)) + '.'))
			]));
};
var $elm$browser$Debugger$Overlay$viewBadMetadata = function (_v0) {
	var message = _v0.message;
	var problems = _v0.problems;
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('The '),
					$elm$browser$Debugger$Overlay$viewCode(message),
					$elm$html$Html$text(' type of your program cannot be reliably serialized for history files.')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:')
				])),
			A2(
			$elm$html$Html$ul,
			_List_Nil,
			A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewProblemType, problems)),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews1),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://guide.elm-lang.org/types/custom_types.html')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('custom types')
						])),
					$elm$html$Html$text(', in your messages. From there, your '),
					$elm$browser$Debugger$Overlay$viewCode('update'),
					$elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews2)
				]))
		]);
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$browser$Debugger$Overlay$Cancel = {$: 'Cancel'};
var $elm$browser$Debugger$Overlay$Proceed = {$: 'Proceed'};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$browser$Debugger$Overlay$viewButtons = function (buttons) {
	var btn = F2(
		function (msg, string) {
			return A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'margin-right', '20px'),
						$elm$html$Html$Events$onClick(msg)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(string)
					]));
		});
	var buttonNodes = function () {
		if (buttons.$ === 'Accept') {
			var proceed = buttons.a;
			return _List_fromArray(
				[
					A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
				]);
		} else {
			var cancel = buttons.a;
			var proceed = buttons.b;
			return _List_fromArray(
				[
					A2(btn, $elm$browser$Debugger$Overlay$Cancel, cancel),
					A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
				]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'height', '60px'),
				A2($elm$html$Html$Attributes$style, 'line-height', '60px'),
				A2($elm$html$Html$Attributes$style, 'text-align', 'right'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
			]),
		buttonNodes);
};
var $elm$browser$Debugger$Overlay$viewMessage = F4(
	function (config, title, details, buttons) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('elm-debugger-overlay'),
					A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
					A2($elm$html$Html$Attributes$style, 'top', '0'),
					A2($elm$html$Html$Attributes$style, 'left', '0'),
					A2($elm$html$Html$Attributes$style, 'width', '100vw'),
					A2($elm$html$Html$Attributes$style, 'height', '100vh'),
					A2($elm$html$Html$Attributes$style, 'color', 'white'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
					A2($elm$html$Html$Attributes$style, 'font-family', '\'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif'),
					A2($elm$html$Html$Attributes$style, 'z-index', '2147483647')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'width', '600px'),
							A2($elm$html$Html$Attributes$style, 'height', '100vh'),
							A2($elm$html$Html$Attributes$style, 'padding-left', 'calc(50% - 300px)'),
							A2($elm$html$Html$Attributes$style, 'padding-right', 'calc(50% - 300px)'),
							A2($elm$html$Html$Attributes$style, 'background-color', 'rgba(200, 200, 200, 0.7)'),
							A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '36px'),
									A2($elm$html$Html$Attributes$style, 'height', '80px'),
									A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)'),
									A2($elm$html$Html$Attributes$style, 'padding-left', '22px'),
									A2($elm$html$Html$Attributes$style, 'vertical-align', 'middle'),
									A2($elm$html$Html$Attributes$style, 'line-height', '80px')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('elm-debugger-details'),
									A2($elm$html$Html$Attributes$style, 'padding', ' 8px 20px'),
									A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
									A2($elm$html$Html$Attributes$style, 'max-height', 'calc(100vh - 156px)'),
									A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(61, 61, 61)')
								]),
							details),
							A2(
							$elm$html$Html$map,
							config.wrap,
							$elm$browser$Debugger$Overlay$viewButtons(buttons))
						]))
				]));
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$browser$Debugger$Overlay$viewShape = F4(
	function (x, y, angle, coordinates) {
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			'http://www.w3.org/2000/svg',
			'polygon',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'points', coordinates),
					A2(
					$elm$virtual_dom$VirtualDom$attribute,
					'transform',
					'translate(' + ($elm$core$String$fromFloat(x) + (' ' + ($elm$core$String$fromFloat(y) + (') rotate(' + ($elm$core$String$fromFloat(-angle) + ')'))))))
				]),
			_List_Nil);
	});
var $elm$browser$Debugger$Overlay$elmLogo = A4(
	$elm$virtual_dom$VirtualDom$nodeNS,
	'http://www.w3.org/2000/svg',
	'svg',
	_List_fromArray(
		[
			A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '-300 -300 600 600'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'currentColor'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'width', '24px'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'height', '24px')
		]),
	_List_fromArray(
		[
			A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			'http://www.w3.org/2000/svg',
			'g',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'transform', 'scale(1 -1)')
				]),
			_List_fromArray(
				[
					A4($elm$browser$Debugger$Overlay$viewShape, 0, -210, 0, '-280,-90 0,190 280,-90'),
					A4($elm$browser$Debugger$Overlay$viewShape, -210, 0, 90, '-280,-90 0,190 280,-90'),
					A4($elm$browser$Debugger$Overlay$viewShape, 207, 207, 45, '-198,-66 0,132 198,-66'),
					A4($elm$browser$Debugger$Overlay$viewShape, 150, 0, 0, '-130,0 0,-130 130,0 0,130'),
					A4($elm$browser$Debugger$Overlay$viewShape, -89, 239, 0, '-191,61 69,61 191,-61 -69,-61'),
					A4($elm$browser$Debugger$Overlay$viewShape, 0, 106, 180, '-130,-44 0,86  130,-44'),
					A4($elm$browser$Debugger$Overlay$viewShape, 256, -150, 270, '-130,-44 0,86  130,-44')
				]))
		]));
var $elm$core$String$length = _String_length;
var $elm$browser$Debugger$Overlay$viewMiniControls = F2(
	function (config, numMsgs) {
		var string = $elm$core$String$fromInt(numMsgs);
		var width = $elm$core$String$fromInt(
			2 + $elm$core$String$length(string));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
					A2($elm$html$Html$Attributes$style, 'bottom', '2em'),
					A2($elm$html$Html$Attributes$style, 'right', '2em'),
					A2($elm$html$Html$Attributes$style, 'width', 'calc(42px + ' + (width + 'ch)')),
					A2($elm$html$Html$Attributes$style, 'height', '36px'),
					A2($elm$html$Html$Attributes$style, 'background-color', '#1293D8'),
					A2($elm$html$Html$Attributes$style, 'color', 'white'),
					A2($elm$html$Html$Attributes$style, 'font-family', 'monospace'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto'),
					A2($elm$html$Html$Attributes$style, 'z-index', '2147483647'),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					$elm$html$Html$Events$onClick(config.open)
				]),
			_List_fromArray(
				[
					$elm$browser$Debugger$Overlay$elmLogo,
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding-left', 'calc(1ch + 6px)'),
							A2($elm$html$Html$Attributes$style, 'padding-right', '1ch')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(string)
						]))
				]));
	});
var $elm$browser$Debugger$Overlay$explanationBad = '\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n';
var $elm$browser$Debugger$Overlay$explanationRisky = '\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n';
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$browser$Debugger$Overlay$viewMention = F2(
	function (tags, verbed) {
		var _v0 = A2(
			$elm$core$List$map,
			$elm$browser$Debugger$Overlay$viewCode,
			$elm$core$List$reverse(tags));
		if (!_v0.b) {
			return $elm$html$Html$text('');
		} else {
			if (!_v0.b.b) {
				var tag = _v0.a;
				return A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(verbed),
							tag,
							$elm$html$Html$text('.')
						]));
			} else {
				if (!_v0.b.b.b) {
					var tag2 = _v0.a;
					var _v1 = _v0.b;
					var tag1 = _v1.a;
					return A2(
						$elm$html$Html$li,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(verbed),
								tag1,
								$elm$html$Html$text(' and '),
								tag2,
								$elm$html$Html$text('.')
							]));
				} else {
					var lastTag = _v0.a;
					var otherTags = _v0.b;
					return A2(
						$elm$html$Html$li,
						_List_Nil,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text(verbed),
							_Utils_ap(
								A2(
									$elm$core$List$intersperse,
									$elm$html$Html$text(', '),
									$elm$core$List$reverse(otherTags)),
								_List_fromArray(
									[
										$elm$html$Html$text(', and '),
										lastTag,
										$elm$html$Html$text('.')
									]))));
				}
			}
		}
	});
var $elm$browser$Debugger$Overlay$viewChange = function (change) {
	return A2(
		$elm$html$Html$li,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'margin', '8px 0')
			]),
		function () {
			if (change.$ === 'AliasChange') {
				var name = change.a;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-size', '1.5em')
							]),
						_List_fromArray(
							[
								$elm$browser$Debugger$Overlay$viewCode(name)
							]))
					]);
			} else {
				var name = change.a;
				var removed = change.b.removed;
				var changed = change.b.changed;
				var added = change.b.added;
				var argsMatch = change.b.argsMatch;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-size', '1.5em')
							]),
						_List_fromArray(
							[
								$elm$browser$Debugger$Overlay$viewCode(name)
							])),
						A2(
						$elm$html$Html$ul,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'list-style-type', 'disc'),
								A2($elm$html$Html$Attributes$style, 'padding-left', '2em')
							]),
						_List_fromArray(
							[
								A2($elm$browser$Debugger$Overlay$viewMention, removed, 'Removed '),
								A2($elm$browser$Debugger$Overlay$viewMention, changed, 'Changed '),
								A2($elm$browser$Debugger$Overlay$viewMention, added, 'Added ')
							])),
						argsMatch ? $elm$html$Html$text('') : $elm$html$Html$text('This may be due to the fact that the type variable names changed.')
					]);
			}
		}());
};
var $elm$browser$Debugger$Overlay$viewReport = F2(
	function (isBad, report) {
		switch (report.$) {
			case 'CorruptHistory':
				return _List_fromArray(
					[
						$elm$html$Html$text('Looks like this history file is corrupt. I cannot understand it.')
					]);
			case 'VersionChanged':
				var old = report.a;
				var _new = report.b;
				return _List_fromArray(
					[
						$elm$html$Html$text('This history was created with Elm ' + (old + (', but you are using Elm ' + (_new + ' right now.'))))
					]);
			case 'MessageChanged':
				var old = report.a;
				var _new = report.b;
				return _List_fromArray(
					[
						$elm$html$Html$text('To import some other history, the overall message type must' + ' be the same. The old history has '),
						$elm$browser$Debugger$Overlay$viewCode(old),
						$elm$html$Html$text(' messages, but the new program works with '),
						$elm$browser$Debugger$Overlay$viewCode(_new),
						$elm$html$Html$text(' messages.')
					]);
			default:
				var changes = report.a;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								isBad ? $elm$browser$Debugger$Overlay$explanationBad : $elm$browser$Debugger$Overlay$explanationRisky)
							])),
						A2(
						$elm$html$Html$ul,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'list-style-type', 'none'),
								A2($elm$html$Html$Attributes$style, 'padding-left', '20px')
							]),
						A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewChange, changes))
					]);
		}
	});
var $elm$browser$Debugger$Overlay$view = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		switch (state.$) {
			case 'None':
				return isOpen ? $elm$html$Html$text('') : (isPaused ? A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('elm-debugger-overlay'),
							A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
							A2($elm$html$Html$Attributes$style, 'top', '0'),
							A2($elm$html$Html$Attributes$style, 'left', '0'),
							A2($elm$html$Html$Attributes$style, 'width', '100vw'),
							A2($elm$html$Html$Attributes$style, 'height', '100vh'),
							A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
							A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
							A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto'),
							A2($elm$html$Html$Attributes$style, 'background-color', 'rgba(200, 200, 200, 0.7)'),
							A2($elm$html$Html$Attributes$style, 'color', 'white'),
							A2($elm$html$Html$Attributes$style, 'font-family', '\'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif'),
							A2($elm$html$Html$Attributes$style, 'z-index', '2147483646'),
							$elm$html$Html$Events$onClick(config.resume)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '80px')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Click to Resume')
								])),
							A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs)
						])) : A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs));
			case 'BadMetadata':
				var badMetadata_ = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Cannot use Import or Export',
					$elm$browser$Debugger$Overlay$viewBadMetadata(badMetadata_),
					$elm$browser$Debugger$Overlay$Accept('Ok'));
			case 'BadImport':
				var report = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Cannot Import History',
					A2($elm$browser$Debugger$Overlay$viewReport, true, report),
					$elm$browser$Debugger$Overlay$Accept('Ok'));
			default:
				var report = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Warning',
					A2($elm$browser$Debugger$Overlay$viewReport, false, report),
					A2($elm$browser$Debugger$Overlay$Choose, 'Cancel', 'Import Anyway'));
		}
	});
var $elm$browser$Debugger$Main$cornerView = function (model) {
	return A5(
		$elm$browser$Debugger$Overlay$view,
		{exportHistory: $elm$browser$Debugger$Main$Export, importHistory: $elm$browser$Debugger$Main$Import, open: $elm$browser$Debugger$Main$Open, resume: $elm$browser$Debugger$Main$Resume, wrap: $elm$browser$Debugger$Main$OverlayMsg},
		$elm$browser$Debugger$Main$isPaused(model.state),
		_Debugger_isOpen(model.popout),
		$elm$browser$Debugger$History$size(model.history),
		model.overlay);
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$foldr = F3(
	function (func, initialState, _v0) {
		var dict = _v0.a;
		return A3(
			$elm$core$Dict$foldr,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $elm$browser$Debugger$Main$getCurrentModel = function (state) {
	if (state.$ === 'Running') {
		var model = state.a;
		return model;
	} else {
		var model = state.b;
		return model;
	}
};
var $elm$browser$Debugger$Main$getUserModel = function (model) {
	return $elm$browser$Debugger$Main$getCurrentModel(model.state);
};
var $elm$browser$Debugger$Main$initialWindowHeight = 420;
var $elm$browser$Debugger$Main$initialWindowWidth = 900;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$browser$Debugger$Main$cachedHistory = function (model) {
	var _v0 = model.state;
	if (_v0.$ === 'Running') {
		return model.history;
	} else {
		var history = _v0.e;
		return history;
	}
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$browser$Debugger$Main$DragEnd = {$: 'DragEnd'};
var $elm$browser$Debugger$Main$getDragStatus = function (layout) {
	if (layout.$ === 'Horizontal') {
		var status = layout.a;
		return status;
	} else {
		var status = layout.a;
		return status;
	}
};
var $elm$browser$Debugger$Main$Drag = function (a) {
	return {$: 'Drag', a: a};
};
var $elm$browser$Debugger$Main$DragInfo = F5(
	function (x, y, down, width, height) {
		return {down: down, height: height, width: width, x: x, y: y};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$browser$Debugger$Main$decodeDimension = function (field) {
	return A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['currentTarget', 'ownerDocument', 'defaultView', field]),
		$elm$json$Json$Decode$float);
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$browser$Debugger$Main$onMouseMove = A2(
	$elm$html$Html$Events$on,
	'mousemove',
	A2(
		$elm$json$Json$Decode$map,
		$elm$browser$Debugger$Main$Drag,
		A6(
			$elm$json$Json$Decode$map5,
			$elm$browser$Debugger$Main$DragInfo,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float),
			A2(
				$elm$json$Json$Decode$field,
				'buttons',
				A2(
					$elm$json$Json$Decode$map,
					function (v) {
						return v === 1;
					},
					$elm$json$Json$Decode$int)),
			$elm$browser$Debugger$Main$decodeDimension('innerWidth'),
			$elm$browser$Debugger$Main$decodeDimension('innerHeight'))));
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$browser$Debugger$Main$toDragListeners = function (layout) {
	var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
	if (_v0.$ === 'Static') {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				$elm$browser$Debugger$Main$onMouseMove,
				$elm$html$Html$Events$onMouseUp($elm$browser$Debugger$Main$DragEnd)
			]);
	}
};
var $elm$browser$Debugger$Main$toFlexDirection = function (layout) {
	if (layout.$ === 'Horizontal') {
		return 'row';
	} else {
		return 'column-reverse';
	}
};
var $elm$browser$Debugger$Main$DragStart = {$: 'DragStart'};
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$browser$Debugger$Main$toPercent = function (fraction) {
	return $elm$core$String$fromFloat(100 * fraction) + '%';
};
var $elm$browser$Debugger$Main$viewDragZone = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2($elm$html$Html$Attributes$style, 'top', '0'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$browser$Debugger$Main$toPercent(x)),
					A2($elm$html$Html$Attributes$style, 'margin-left', '-5px'),
					A2($elm$html$Html$Attributes$style, 'width', '10px'),
					A2($elm$html$Html$Attributes$style, 'height', '100%'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'col-resize'),
					$elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
				]),
			_List_Nil);
	} else {
		var y = layout.c;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$browser$Debugger$Main$toPercent(y)),
					A2($elm$html$Html$Attributes$style, 'left', '0'),
					A2($elm$html$Html$Attributes$style, 'margin-top', '-5px'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '10px'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'row-resize'),
					$elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
				]),
			_List_Nil);
	}
};
var $elm$browser$Debugger$Main$TweakExpandoModel = function (a) {
	return {$: 'TweakExpandoModel', a: a};
};
var $elm$browser$Debugger$Main$TweakExpandoMsg = function (a) {
	return {$: 'TweakExpandoMsg', a: a};
};
var $elm$browser$Debugger$Main$toExpandoPercents = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return _Utils_Tuple2(
			$elm$browser$Debugger$Main$toPercent(1 - x),
			'100%');
	} else {
		var y = layout.c;
		return _Utils_Tuple2(
			'100%',
			$elm$browser$Debugger$Main$toPercent(y));
	}
};
var $elm$browser$Debugger$Main$toMouseBlocker = function (layout) {
	var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
	if (_v0.$ === 'Static') {
		return 'auto';
	} else {
		return 'none';
	}
};
var $elm$browser$Debugger$Expando$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$browser$Debugger$Expando$Index = F3(
	function (a, b, c) {
		return {$: 'Index', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$Key = {$: 'Key'};
var $elm$browser$Debugger$Expando$None = {$: 'None'};
var $elm$browser$Debugger$Expando$Toggle = {$: 'Toggle'};
var $elm$browser$Debugger$Expando$Value = {$: 'Value'};
var $elm$browser$Debugger$Expando$blue = A2($elm$html$Html$Attributes$style, 'color', 'rgb(28, 0, 207)');
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Debugger$Expando$leftPad = function (maybeKey) {
	if (maybeKey.$ === 'Nothing') {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'padding-left', '4ch')
			]);
	}
};
var $elm$browser$Debugger$Expando$makeArrow = function (arrow) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'color', '#777'),
				A2($elm$html$Html$Attributes$style, 'padding-left', '2ch'),
				A2($elm$html$Html$Attributes$style, 'width', '2ch'),
				A2($elm$html$Html$Attributes$style, 'display', 'inline-block')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(arrow)
			]));
};
var $elm$browser$Debugger$Expando$purple = A2($elm$html$Html$Attributes$style, 'color', 'rgb(136, 19, 145)');
var $elm$browser$Debugger$Expando$lineStarter = F3(
	function (maybeKey, maybeIsClosed, description) {
		var arrow = function () {
			if (maybeIsClosed.$ === 'Nothing') {
				return $elm$browser$Debugger$Expando$makeArrow('');
			} else {
				if (maybeIsClosed.a) {
					return $elm$browser$Debugger$Expando$makeArrow('▸');
				} else {
					return $elm$browser$Debugger$Expando$makeArrow('▾');
				}
			}
		}();
		if (maybeKey.$ === 'Nothing') {
			return A2($elm$core$List$cons, arrow, description);
		} else {
			var key = maybeKey.a;
			return A2(
				$elm$core$List$cons,
				arrow,
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$purple]),
						_List_fromArray(
							[
								$elm$html$Html$text(key)
							])),
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(' = '),
						description)));
		}
	});
var $elm$browser$Debugger$Expando$red = A2($elm$html$Html$Attributes$style, 'color', 'rgb(196, 26, 22)');
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$browser$Debugger$Expando$seqTypeToString = F2(
	function (n, seqType) {
		switch (seqType.$) {
			case 'ListSeq':
				return 'List(' + ($elm$core$String$fromInt(n) + ')');
			case 'SetSeq':
				return 'Set(' + ($elm$core$String$fromInt(n) + ')');
			default:
				return 'Array(' + ($elm$core$String$fromInt(n) + ')');
		}
	});
var $elm$core$String$slice = _String_slice;
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $elm$browser$Debugger$Expando$elideMiddle = function (str) {
	return ($elm$core$String$length(str) <= 18) ? str : (A2($elm$core$String$left, 8, str) + ('...' + A2($elm$core$String$right, 8, str)));
};
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$browser$Debugger$Expando$viewExtraTinyRecord = F3(
	function (length, starter, entries) {
		if (!entries.b) {
			return _Utils_Tuple2(
				length + 1,
				_List_fromArray(
					[
						$elm$html$Html$text('}')
					]));
		} else {
			var field = entries.a;
			var rest = entries.b;
			var nextLength = (length + $elm$core$String$length(field)) + 1;
			if (nextLength > 18) {
				return _Utils_Tuple2(
					length + 2,
					_List_fromArray(
						[
							$elm$html$Html$text('…}')
						]));
			} else {
				var _v1 = A3($elm$browser$Debugger$Expando$viewExtraTinyRecord, nextLength, ',', rest);
				var finalLength = _v1.a;
				var otherHtmls = _v1.b;
				return _Utils_Tuple2(
					finalLength,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(starter),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$purple]),
								_List_fromArray(
									[
										$elm$html$Html$text(field)
									])),
							otherHtmls)));
			}
		}
	});
var $elm$browser$Debugger$Expando$viewTinyHelp = function (str) {
	return _Utils_Tuple2(
		$elm$core$String$length(str),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $elm$browser$Debugger$Expando$viewExtraTiny = function (value) {
	if (value.$ === 'Record') {
		var record = value.b;
		return A3(
			$elm$browser$Debugger$Expando$viewExtraTinyRecord,
			0,
			'{',
			$elm$core$Dict$keys(record));
	} else {
		return $elm$browser$Debugger$Expando$viewTiny(value);
	}
};
var $elm$browser$Debugger$Expando$viewTiny = function (value) {
	switch (value.$) {
		case 'S':
			var stringRep = value.a;
			var str = $elm$browser$Debugger$Expando$elideMiddle(stringRep);
			return _Utils_Tuple2(
				$elm$core$String$length(str),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$red]),
						_List_fromArray(
							[
								$elm$html$Html$text(str)
							]))
					]));
		case 'Primitive':
			var stringRep = value.a;
			return _Utils_Tuple2(
				$elm$core$String$length(stringRep),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$blue]),
						_List_fromArray(
							[
								$elm$html$Html$text(stringRep)
							]))
					]));
		case 'Sequence':
			var seqType = value.a;
			var valueList = value.c;
			return $elm$browser$Debugger$Expando$viewTinyHelp(
				A2(
					$elm$browser$Debugger$Expando$seqTypeToString,
					$elm$core$List$length(valueList),
					seqType));
		case 'Dictionary':
			var keyValuePairs = value.b;
			return $elm$browser$Debugger$Expando$viewTinyHelp(
				'Dict(' + ($elm$core$String$fromInt(
					$elm$core$List$length(keyValuePairs)) + ')'));
		case 'Record':
			var record = value.b;
			return $elm$browser$Debugger$Expando$viewTinyRecord(record);
		default:
			if (!value.c.b) {
				var maybeName = value.a;
				return $elm$browser$Debugger$Expando$viewTinyHelp(
					A2($elm$core$Maybe$withDefault, 'Unit', maybeName));
			} else {
				var maybeName = value.a;
				var valueList = value.c;
				return $elm$browser$Debugger$Expando$viewTinyHelp(
					function () {
						if (maybeName.$ === 'Nothing') {
							return 'Tuple(' + ($elm$core$String$fromInt(
								$elm$core$List$length(valueList)) + ')');
						} else {
							var name = maybeName.a;
							return name + ' …';
						}
					}());
			}
	}
};
var $elm$browser$Debugger$Expando$viewTinyRecord = function (record) {
	return $elm$core$Dict$isEmpty(record) ? _Utils_Tuple2(
		2,
		_List_fromArray(
			[
				$elm$html$Html$text('{}')
			])) : A3(
		$elm$browser$Debugger$Expando$viewTinyRecordHelp,
		0,
		'{ ',
		$elm$core$Dict$toList(record));
};
var $elm$browser$Debugger$Expando$viewTinyRecordHelp = F3(
	function (length, starter, entries) {
		if (!entries.b) {
			return _Utils_Tuple2(
				length + 2,
				_List_fromArray(
					[
						$elm$html$Html$text(' }')
					]));
		} else {
			var _v1 = entries.a;
			var field = _v1.a;
			var value = _v1.b;
			var rest = entries.b;
			var fieldLen = $elm$core$String$length(field);
			var _v2 = $elm$browser$Debugger$Expando$viewExtraTiny(value);
			var valueLen = _v2.a;
			var valueHtmls = _v2.b;
			var newLength = ((length + fieldLen) + valueLen) + 5;
			if (newLength > 60) {
				return _Utils_Tuple2(
					length + 4,
					_List_fromArray(
						[
							$elm$html$Html$text(', … }')
						]));
			} else {
				var _v3 = A3($elm$browser$Debugger$Expando$viewTinyRecordHelp, newLength, ', ', rest);
				var finalLength = _v3.a;
				var otherHtmls = _v3.b;
				return _Utils_Tuple2(
					finalLength,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(starter),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$purple]),
								_List_fromArray(
									[
										$elm$html$Html$text(field)
									])),
							A2(
								$elm$core$List$cons,
								$elm$html$Html$text(' = '),
								A2(
									$elm$core$List$cons,
									A2($elm$html$Html$span, _List_Nil, valueHtmls),
									otherHtmls)))));
			}
		}
	});
var $elm$browser$Debugger$Expando$view = F2(
	function (maybeKey, expando) {
		switch (expando.$) {
			case 'S':
				var stringRep = expando.a;
				return A2(
					$elm$html$Html$div,
					$elm$browser$Debugger$Expando$leftPad(maybeKey),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Nothing,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$red]),
								_List_fromArray(
									[
										$elm$html$Html$text(stringRep)
									]))
							])));
			case 'Primitive':
				var stringRep = expando.a;
				return A2(
					$elm$html$Html$div,
					$elm$browser$Debugger$Expando$leftPad(maybeKey),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Nothing,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$blue]),
								_List_fromArray(
									[
										$elm$html$Html$text(stringRep)
									]))
							])));
			case 'Sequence':
				var seqType = expando.a;
				var isClosed = expando.b;
				var valueList = expando.c;
				return A4($elm$browser$Debugger$Expando$viewSequence, maybeKey, seqType, isClosed, valueList);
			case 'Dictionary':
				var isClosed = expando.a;
				var keyValuePairs = expando.b;
				return A3($elm$browser$Debugger$Expando$viewDictionary, maybeKey, isClosed, keyValuePairs);
			case 'Record':
				var isClosed = expando.a;
				var valueDict = expando.b;
				return A3($elm$browser$Debugger$Expando$viewRecord, maybeKey, isClosed, valueDict);
			default:
				var maybeName = expando.a;
				var isClosed = expando.b;
				var valueList = expando.c;
				return A4($elm$browser$Debugger$Expando$viewConstructor, maybeKey, maybeName, isClosed, valueList);
		}
	});
var $elm$browser$Debugger$Expando$viewConstructor = F4(
	function (maybeKey, maybeName, isClosed, valueList) {
		var tinyArgs = A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeL, $elm$core$Tuple$second, $elm$browser$Debugger$Expando$viewExtraTiny),
			valueList);
		var description = function () {
			var _v7 = _Utils_Tuple2(maybeName, tinyArgs);
			if (_v7.a.$ === 'Nothing') {
				if (!_v7.b.b) {
					var _v8 = _v7.a;
					return _List_fromArray(
						[
							$elm$html$Html$text('()')
						]);
				} else {
					var _v9 = _v7.a;
					var _v10 = _v7.b;
					var x = _v10.a;
					var xs = _v10.b;
					return A2(
						$elm$core$List$cons,
						$elm$html$Html$text('( '),
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$span, _List_Nil, x),
							A3(
								$elm$core$List$foldr,
								F2(
									function (args, rest) {
										return A2(
											$elm$core$List$cons,
											$elm$html$Html$text(', '),
											A2(
												$elm$core$List$cons,
												A2($elm$html$Html$span, _List_Nil, args),
												rest));
									}),
								_List_fromArray(
									[
										$elm$html$Html$text(' )')
									]),
								xs)));
				}
			} else {
				if (!_v7.b.b) {
					var name = _v7.a.a;
					return _List_fromArray(
						[
							$elm$html$Html$text(name)
						]);
				} else {
					var name = _v7.a.a;
					var _v11 = _v7.b;
					var x = _v11.a;
					var xs = _v11.b;
					return A2(
						$elm$core$List$cons,
						$elm$html$Html$text(name + ' '),
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$span, _List_Nil, x),
							A3(
								$elm$core$List$foldr,
								F2(
									function (args, rest) {
										return A2(
											$elm$core$List$cons,
											$elm$html$Html$text(' '),
											A2(
												$elm$core$List$cons,
												A2($elm$html$Html$span, _List_Nil, args),
												rest));
									}),
								_List_Nil,
								xs)));
				}
			}
		}();
		var _v4 = function () {
			if (!valueList.b) {
				return _Utils_Tuple2(
					$elm$core$Maybe$Nothing,
					A2($elm$html$Html$div, _List_Nil, _List_Nil));
			} else {
				if (!valueList.b.b) {
					var entry = valueList.a;
					switch (entry.$) {
						case 'S':
							return _Utils_Tuple2(
								$elm$core$Maybe$Nothing,
								A2($elm$html$Html$div, _List_Nil, _List_Nil));
						case 'Primitive':
							return _Utils_Tuple2(
								$elm$core$Maybe$Nothing,
								A2($elm$html$Html$div, _List_Nil, _List_Nil));
						case 'Sequence':
							var subValueList = entry.c;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewSequenceOpen(subValueList)));
						case 'Dictionary':
							var keyValuePairs = entry.b;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)));
						case 'Record':
							var record = entry.b;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewRecordOpen(record)));
						default:
							var subValueList = entry.c;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewConstructorOpen(subValueList)));
					}
				} else {
					return _Utils_Tuple2(
						$elm$core$Maybe$Just(isClosed),
						isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : $elm$browser$Debugger$Expando$viewConstructorOpen(valueList));
				}
			}
		}();
		var maybeIsClosed = _v4.a;
		var openHtml = _v4.b;
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, maybeIsClosed, description)),
					openHtml
				]));
	});
var $elm$browser$Debugger$Expando$viewConstructorEntry = F2(
	function (index, value) {
		return A2(
			$elm$html$Html$map,
			A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, index),
			A2(
				$elm$browser$Debugger$Expando$view,
				$elm$core$Maybe$Just(
					$elm$core$String$fromInt(index)),
				value));
	});
var $elm$browser$Debugger$Expando$viewConstructorOpen = function (valueList) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, valueList));
};
var $elm$browser$Debugger$Expando$viewDictionary = F3(
	function (maybeKey, isClosed, keyValuePairs) {
		var starter = 'Dict(' + ($elm$core$String$fromInt(
			$elm$core$List$length(keyValuePairs)) + ')');
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						_List_fromArray(
							[
								$elm$html$Html$text(starter)
							]))),
					isClosed ? $elm$html$Html$text('') : $elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)
				]));
	});
var $elm$browser$Debugger$Expando$viewDictionaryEntry = F2(
	function (index, _v2) {
		var key = _v2.a;
		var value = _v2.b;
		switch (key.$) {
			case 'S':
				var stringRep = key.a;
				return A2(
					$elm$html$Html$map,
					A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
					A2(
						$elm$browser$Debugger$Expando$view,
						$elm$core$Maybe$Just(stringRep),
						value));
			case 'Primitive':
				var stringRep = key.a;
				return A2(
					$elm$html$Html$map,
					A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
					A2(
						$elm$browser$Debugger$Expando$view,
						$elm$core$Maybe$Just(stringRep),
						value));
			default:
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$map,
							A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Key, index),
							A2(
								$elm$browser$Debugger$Expando$view,
								$elm$core$Maybe$Just('key'),
								key)),
							A2(
							$elm$html$Html$map,
							A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
							A2(
								$elm$browser$Debugger$Expando$view,
								$elm$core$Maybe$Just('value'),
								value))
						]));
		}
	});
var $elm$browser$Debugger$Expando$viewDictionaryOpen = function (keyValuePairs) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewDictionaryEntry, keyValuePairs));
};
var $elm$browser$Debugger$Expando$viewRecord = F3(
	function (maybeKey, isClosed, record) {
		var _v1 = isClosed ? _Utils_Tuple3(
			$elm$browser$Debugger$Expando$viewTinyRecord(record).b,
			$elm$html$Html$text(''),
			$elm$html$Html$text('')) : _Utils_Tuple3(
			_List_fromArray(
				[
					$elm$html$Html$text('{')
				]),
			$elm$browser$Debugger$Expando$viewRecordOpen(record),
			A2(
				$elm$html$Html$div,
				$elm$browser$Debugger$Expando$leftPad(
					$elm$core$Maybe$Just(_Utils_Tuple0)),
				_List_fromArray(
					[
						$elm$html$Html$text('}')
					])));
		var start = _v1.a;
		var middle = _v1.b;
		var end = _v1.c;
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						start)),
					middle,
					end
				]));
	});
var $elm$browser$Debugger$Expando$viewRecordEntry = function (_v0) {
	var field = _v0.a;
	var value = _v0.b;
	return A2(
		$elm$html$Html$map,
		$elm$browser$Debugger$Expando$Field(field),
		A2(
			$elm$browser$Debugger$Expando$view,
			$elm$core$Maybe$Just(field),
			value));
};
var $elm$browser$Debugger$Expando$viewRecordOpen = function (record) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2(
			$elm$core$List$map,
			$elm$browser$Debugger$Expando$viewRecordEntry,
			$elm$core$Dict$toList(record)));
};
var $elm$browser$Debugger$Expando$viewSequence = F4(
	function (maybeKey, seqType, isClosed, valueList) {
		var starter = A2(
			$elm$browser$Debugger$Expando$seqTypeToString,
			$elm$core$List$length(valueList),
			seqType);
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						_List_fromArray(
							[
								$elm$html$Html$text(starter)
							]))),
					isClosed ? $elm$html$Html$text('') : $elm$browser$Debugger$Expando$viewSequenceOpen(valueList)
				]));
	});
var $elm$browser$Debugger$Expando$viewSequenceOpen = function (values) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, values));
};
var $elm$browser$Debugger$Main$viewExpando = F3(
	function (expandoMsg, expandoModel, layout) {
		var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
		var _v0 = $elm$browser$Debugger$Main$toExpandoPercents(layout);
		var w = _v0.a;
		var h = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'block'),
					A2($elm$html$Html$Attributes$style, 'width', 'calc(' + (w + ' - 4em)')),
					A2($elm$html$Html$Attributes$style, 'height', 'calc(' + (h + ' - 4em)')),
					A2($elm$html$Html$Attributes$style, 'padding', '2em'),
					A2($elm$html$Html$Attributes$style, 'margin', '0'),
					A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', block),
					A2($elm$html$Html$Attributes$style, '-webkit-user-select', block),
					A2($elm$html$Html$Attributes$style, '-moz-user-select', block),
					A2($elm$html$Html$Attributes$style, '-ms-user-select', block),
					A2($elm$html$Html$Attributes$style, 'user-select', block)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#ccc'),
							A2($elm$html$Html$Attributes$style, 'padding', '0 0 1em 0')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('-- MESSAGE')
						])),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$TweakExpandoMsg,
					A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoMsg)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#ccc'),
							A2($elm$html$Html$Attributes$style, 'padding', '1em 0')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('-- MODEL')
						])),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$TweakExpandoModel,
					A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoModel))
				]));
	});
var $elm$browser$Debugger$Main$Jump = function (a) {
	return {$: 'Jump', a: a};
};
var $elm$virtual_dom$VirtualDom$lazy = _VirtualDom_lazy;
var $elm$html$Html$Lazy$lazy = $elm$virtual_dom$VirtualDom$lazy;
var $elm$browser$Debugger$Main$toHistoryPercents = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return _Utils_Tuple2(
			$elm$browser$Debugger$Main$toPercent(x),
			'100%');
	} else {
		var y = layout.c;
		return _Utils_Tuple2(
			'100%',
			$elm$browser$Debugger$Main$toPercent(1 - y));
	}
};
var $elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
var $elm$html$Html$Lazy$lazy3 = $elm$virtual_dom$VirtualDom$lazy3;
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$browser$Debugger$History$idForMessageIndex = function (index) {
	return 'msg-' + $elm$core$String$fromInt(index);
};
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $elm$browser$Debugger$History$viewMessage = F3(
	function (currentIndex, index, msg) {
		var messageName = _Debugger_messageToString(msg);
		var className = _Utils_eq(currentIndex, index) ? 'elm-debugger-entry elm-debugger-entry-selected' : 'elm-debugger-entry';
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id(
					$elm$browser$Debugger$History$idForMessageIndex(index)),
					$elm$html$Html$Attributes$class(className),
					$elm$html$Html$Events$onClick(index)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$title(messageName),
							$elm$html$Html$Attributes$class('elm-debugger-entry-content')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(messageName)
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-debugger-entry-index')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(index))
						]))
				]));
	});
var $elm$browser$Debugger$History$consMsg = F3(
	function (currentIndex, msg, _v0) {
		var index = _v0.a;
		var rest = _v0.b;
		return _Utils_Tuple2(
			index + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$elm$core$String$fromInt(index),
					A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewMessage, currentIndex, index, msg)),
				rest));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$browser$Debugger$History$maxSnapshotSize = 31;
var $elm$browser$Debugger$History$showMoreButton = function (numMessages) {
	var nextIndex = (numMessages - 1) - ($elm$browser$Debugger$History$maxSnapshotSize * 2);
	var labelText = 'View more messages';
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('elm-debugger-entry'),
				$elm$html$Html$Events$onClick(nextIndex)
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$title(labelText),
						$elm$html$Html$Attributes$class('elm-debugger-entry-content')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(labelText)
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-debugger-entry-index')
					]),
				_List_Nil)
			]));
};
var $elm$browser$Debugger$History$styles = A3(
	$elm$html$Html$node,
	'style',
	_List_Nil,
	_List_fromArray(
		[
			$elm$html$Html$text('\n\n.elm-debugger-entry {\n  cursor: pointer;\n  width: 100%;\n  box-sizing: border-box;\n  padding: 8px;\n}\n\n.elm-debugger-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.elm-debugger-entry-selected, .elm-debugger-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.elm-debugger-entry-content {\n  width: calc(100% - 40px);\n  padding: 0 5px;\n  box-sizing: border-box;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.elm-debugger-entry-index {\n  color: #666;\n  width: 40px;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n')
		]));
var $elm$core$Basics$ge = _Utils_ge;
var $elm$browser$Debugger$History$viewSnapshot = F3(
	function (selectedIndex, index, _v0) {
		var messages = _v0.messages;
		return A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_Nil,
			A3(
				$elm$core$Array$foldr,
				$elm$browser$Debugger$History$consMsg(selectedIndex),
				_Utils_Tuple2(index, _List_Nil),
				messages).b);
	});
var $elm$browser$Debugger$History$consSnapshot = F3(
	function (selectedIndex, snapshot, _v0) {
		var index = _v0.a;
		var rest = _v0.b;
		var nextIndex = index + $elm$core$Array$length(snapshot.messages);
		var selectedIndexHelp = ((_Utils_cmp(nextIndex, selectedIndex) > 0) && (_Utils_cmp(selectedIndex, index) > -1)) ? selectedIndex : (-1);
		return _Utils_Tuple2(
			nextIndex,
			A2(
				$elm$core$List$cons,
				A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewSnapshot, selectedIndexHelp, index, snapshot),
				rest));
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $elm$browser$Debugger$History$viewAllSnapshots = F3(
	function (selectedIndex, startIndex, snapshots) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			A3(
				$elm$core$Array$foldl,
				$elm$browser$Debugger$History$consSnapshot(selectedIndex),
				_Utils_Tuple2(startIndex, _List_Nil),
				snapshots).b);
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $elm$browser$Debugger$History$viewRecentSnapshots = F3(
	function (selectedIndex, recentMessagesNum, snapshots) {
		var messagesToFill = $elm$browser$Debugger$History$maxSnapshotSize - recentMessagesNum;
		var arrayLength = $elm$core$Array$length(snapshots);
		var snapshotsToRender = function () {
			var _v0 = _Utils_Tuple2(
				A2($elm$core$Array$get, arrayLength - 2, snapshots),
				A2($elm$core$Array$get, arrayLength - 1, snapshots));
			if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
				var fillerSnapshot = _v0.a.a;
				var recentSnapshot = _v0.b.a;
				return $elm$core$Array$fromList(
					_List_fromArray(
						[
							{
							messages: A3($elm$core$Array$slice, 0, messagesToFill, fillerSnapshot.messages),
							model: fillerSnapshot.model
						},
							recentSnapshot
						]));
			} else {
				return snapshots;
			}
		}();
		var startingIndex = ((arrayLength * $elm$browser$Debugger$History$maxSnapshotSize) - $elm$browser$Debugger$History$maxSnapshotSize) - messagesToFill;
		return A3($elm$browser$Debugger$History$viewAllSnapshots, selectedIndex, startingIndex, snapshotsToRender);
	});
var $elm$browser$Debugger$History$view = F2(
	function (maybeIndex, _v0) {
		var snapshots = _v0.snapshots;
		var recent = _v0.recent;
		var numMessages = _v0.numMessages;
		var recentMessageStartIndex = numMessages - recent.numMessages;
		var index = A2($elm$core$Maybe$withDefault, -1, maybeIndex);
		var newStuff = A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_Nil,
			A3(
				$elm$core$List$foldr,
				$elm$browser$Debugger$History$consMsg(index),
				_Utils_Tuple2(recentMessageStartIndex, _List_Nil),
				recent.messages).b);
		var onlyRenderRecentMessages = (!_Utils_eq(index, -1)) || ($elm$core$Array$length(snapshots) < 2);
		var oldStuff = onlyRenderRecentMessages ? A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewAllSnapshots, index, 0, snapshots) : A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewRecentSnapshots, index, recent.numMessages, snapshots);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('elm-debugger-sidebar'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
					A2($elm$html$Html$Attributes$style, 'height', 'calc(100% - 72px)')
				]),
			A2(
				$elm$core$List$cons,
				$elm$browser$Debugger$History$styles,
				A2(
					$elm$core$List$cons,
					newStuff,
					A2(
						$elm$core$List$cons,
						oldStuff,
						onlyRenderRecentMessages ? _List_Nil : _List_fromArray(
							[
								$elm$browser$Debugger$History$showMoreButton(numMessages)
							])))));
	});
var $elm$browser$Debugger$Main$SwapLayout = {$: 'SwapLayout'};
var $elm$browser$Debugger$Main$toHistoryIcon = function (layout) {
	if (layout.$ === 'Horizontal') {
		return 'M13 1a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M13 3h-10a1 1 0 0 0-1 1v5h12v-5a1 1 0 0 0-1-1z M14 10h-12v2a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1z';
	} else {
		return 'M0 4a3 3 0 0 1 3-3h10a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3z M2 4v8a1 1 0 0 0 1 1h2v-10h-2a1 1 0 0 0-1 1z M6 3v10h7a1 1 0 0 0 1-1v-8a1 1 0 0 0-1-1z';
	}
};
var $elm$browser$Debugger$Main$icon = function (path) {
	return A4(
		$elm$virtual_dom$VirtualDom$nodeNS,
		'http://www.w3.org/2000/svg',
		'svg',
		_List_fromArray(
			[
				A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '0 0 16 16'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'currentColor'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'width', '16px'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'height', '16px')
			]),
		_List_fromArray(
			[
				A4(
				$elm$virtual_dom$VirtualDom$nodeNS,
				'http://www.w3.org/2000/svg',
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', path)
					]),
				_List_Nil)
			]));
};
var $elm$browser$Debugger$Main$viewHistoryButton = F3(
	function (label, msg, path) {
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'background', 'none'),
					A2($elm$html$Html$Attributes$style, 'border', 'none'),
					A2($elm$html$Html$Attributes$style, 'color', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					$elm$html$Html$Events$onClick(msg)
				]),
			_List_fromArray(
				[
					$elm$browser$Debugger$Main$icon(path),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding-left', '6px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						]))
				]));
	});
var $elm$browser$Debugger$Main$viewHistoryOptions = function (layout) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'height', '36px'),
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
				A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
				A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
			]),
		_List_fromArray(
			[
				A3(
				$elm$browser$Debugger$Main$viewHistoryButton,
				'Swap Layout',
				$elm$browser$Debugger$Main$SwapLayout,
				$elm$browser$Debugger$Main$toHistoryIcon(layout)),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
						A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between')
					]),
				_List_fromArray(
					[
						A3($elm$browser$Debugger$Main$viewHistoryButton, 'Import', $elm$browser$Debugger$Main$Import, 'M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M10 2a1 1 0 0 0 -2 0v6a1 1 0 0 0 1 1h6a1 1 0 0 0 0-2h-3.586l4.293-4.293a1 1 0 0 0-1.414-1.414l-4.293 4.293z'),
						A3($elm$browser$Debugger$Main$viewHistoryButton, 'Export', $elm$browser$Debugger$Main$Export, 'M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1 a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M9 3a1 1 0 1 1 0-2h6a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-3.586l-5.293 5.293 a1 1 0 0 1-1.414-1.414l5.293 -5.293z')
					]))
			]));
};
var $elm$browser$Debugger$Main$SliderJump = function (a) {
	return {$: 'SliderJump', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$browser$Debugger$Main$isPlaying = function (maybeIndex) {
	if (maybeIndex.$ === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$core$String$toInt = _String_toInt;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $elm$browser$Debugger$Main$viewPlayButton = function (playing) {
	return A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'background', '#1293D8'),
				A2($elm$html$Html$Attributes$style, 'border', 'none'),
				A2($elm$html$Html$Attributes$style, 'color', 'white'),
				A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
				A2($elm$html$Html$Attributes$style, 'width', '36px'),
				A2($elm$html$Html$Attributes$style, 'height', '36px'),
				$elm$html$Html$Events$onClick($elm$browser$Debugger$Main$Resume)
			]),
		_List_fromArray(
			[
				playing ? $elm$browser$Debugger$Main$icon('M2 2h4v12h-4v-12z M10 2h4v12h-4v-12z') : $elm$browser$Debugger$Main$icon('M2 2l12 7l-12 7z')
			]));
};
var $elm$browser$Debugger$Main$viewHistorySlider = F2(
	function (history, maybeIndex) {
		var lastIndex = $elm$browser$Debugger$History$size(history) - 1;
		var selectedIndex = A2($elm$core$Maybe$withDefault, lastIndex, maybeIndex);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '36px'),
					A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Lazy$lazy,
					$elm$browser$Debugger$Main$viewPlayButton,
					$elm$browser$Debugger$Main$isPlaying(maybeIndex)),
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('range'),
							A2($elm$html$Html$Attributes$style, 'width', 'calc(100% - 56px)'),
							A2($elm$html$Html$Attributes$style, 'height', '36px'),
							A2($elm$html$Html$Attributes$style, 'margin', '0 10px'),
							$elm$html$Html$Attributes$min('0'),
							$elm$html$Html$Attributes$max(
							$elm$core$String$fromInt(lastIndex)),
							$elm$html$Html$Attributes$value(
							$elm$core$String$fromInt(selectedIndex)),
							$elm$html$Html$Events$onInput(
							A2(
								$elm$core$Basics$composeR,
								$elm$core$String$toInt,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$withDefault(lastIndex),
									$elm$browser$Debugger$Main$SliderJump)))
						]),
					_List_Nil)
				]));
	});
var $elm$browser$Debugger$Main$viewHistory = F3(
	function (maybeIndex, history, layout) {
		var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
		var _v0 = $elm$browser$Debugger$Main$toHistoryPercents(layout);
		var w = _v0.a;
		var h = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'width', w),
					A2($elm$html$Html$Attributes$style, 'height', h),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
					A2($elm$html$Html$Attributes$style, 'color', '#DDDDDD'),
					A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(61, 61, 61)'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', block),
					A2($elm$html$Html$Attributes$style, 'user-select', block)
				]),
			_List_fromArray(
				[
					A2($elm$browser$Debugger$Main$viewHistorySlider, history, maybeIndex),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$Jump,
					A2($elm$browser$Debugger$History$view, maybeIndex, history)),
					A2($elm$html$Html$Lazy$lazy, $elm$browser$Debugger$Main$viewHistoryOptions, layout)
				]));
	});
var $elm$browser$Debugger$Main$popoutView = function (model) {
	var maybeIndex = function () {
		var _v0 = model.state;
		if (_v0.$ === 'Running') {
			return $elm$core$Maybe$Nothing;
		} else {
			var index = _v0.a;
			return $elm$core$Maybe$Just(index);
		}
	}();
	var historyToRender = $elm$browser$Debugger$Main$cachedHistory(model);
	return A3(
		$elm$html$Html$node,
		'body',
		_Utils_ap(
			$elm$browser$Debugger$Main$toDragListeners(model.layout),
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'margin', '0'),
					A2($elm$html$Html$Attributes$style, 'padding', '0'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '100%'),
					A2($elm$html$Html$Attributes$style, 'font-family', 'monospace'),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(
					$elm$html$Html$Attributes$style,
					'flex-direction',
					$elm$browser$Debugger$Main$toFlexDirection(model.layout))
				])),
		_List_fromArray(
			[
				A3($elm$browser$Debugger$Main$viewHistory, maybeIndex, historyToRender, model.layout),
				$elm$browser$Debugger$Main$viewDragZone(model.layout),
				A3($elm$browser$Debugger$Main$viewExpando, model.expandoMsg, model.expandoModel, model.layout)
			]));
};
var $elm$browser$Debugger$Overlay$BlockAll = {$: 'BlockAll'};
var $elm$browser$Debugger$Overlay$toBlockerType = F2(
	function (isPaused, state) {
		switch (state.$) {
			case 'None':
				return isPaused ? $elm$browser$Debugger$Overlay$BlockAll : $elm$browser$Debugger$Overlay$BlockNone;
			case 'BadMetadata':
				return $elm$browser$Debugger$Overlay$BlockMost;
			case 'BadImport':
				return $elm$browser$Debugger$Overlay$BlockMost;
			default:
				return $elm$browser$Debugger$Overlay$BlockMost;
		}
	});
var $elm$browser$Debugger$Main$toBlockerType = function (model) {
	return A2(
		$elm$browser$Debugger$Overlay$toBlockerType,
		$elm$browser$Debugger$Main$isPaused(model.state),
		model.overlay);
};
var $elm$browser$Debugger$Main$Horizontal = F3(
	function (a, b, c) {
		return {$: 'Horizontal', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Main$Running = function (a) {
	return {$: 'Running', a: a};
};
var $elm$browser$Debugger$Main$Static = {$: 'Static'};
var $elm$browser$Debugger$Metadata$Error = F2(
	function (message, problems) {
		return {message: message, problems: problems};
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$browser$Debugger$Metadata$Metadata = F2(
	function (versions, types) {
		return {types: types, versions: versions};
	});
var $elm$browser$Debugger$Metadata$Types = F3(
	function (message, aliases, unions) {
		return {aliases: aliases, message: message, unions: unions};
	});
var $elm$browser$Debugger$Metadata$Alias = F2(
	function (args, tipe) {
		return {args: args, tipe: tipe};
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$browser$Debugger$Metadata$decodeAlias = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Alias,
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $elm$browser$Debugger$Metadata$Union = F2(
	function (args, tags) {
		return {args: args, tags: tags};
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$browser$Debugger$Metadata$decodeUnion = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Union,
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'tags',
		$elm$json$Json$Decode$dict(
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$browser$Debugger$Metadata$decodeTypes = A4(
	$elm$json$Json$Decode$map3,
	$elm$browser$Debugger$Metadata$Types,
	A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'aliases',
		$elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeAlias)),
	A2(
		$elm$json$Json$Decode$field,
		'unions',
		$elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeUnion)));
var $elm$browser$Debugger$Metadata$Versions = function (elm) {
	return {elm: elm};
};
var $elm$browser$Debugger$Metadata$decodeVersions = A2(
	$elm$json$Json$Decode$map,
	$elm$browser$Debugger$Metadata$Versions,
	A2($elm$json$Json$Decode$field, 'elm', $elm$json$Json$Decode$string));
var $elm$browser$Debugger$Metadata$decoder = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Metadata,
	A2($elm$json$Json$Decode$field, 'versions', $elm$browser$Debugger$Metadata$decodeVersions),
	A2($elm$json$Json$Decode$field, 'types', $elm$browser$Debugger$Metadata$decodeTypes));
var $elm$browser$Debugger$Metadata$ProblemType = F2(
	function (name, problems) {
		return {name: name, problems: problems};
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$String$contains = _String_contains;
var $elm$browser$Debugger$Metadata$hasProblem = F2(
	function (tipe, _v0) {
		var problem = _v0.a;
		var token = _v0.b;
		return A2($elm$core$String$contains, token, tipe) ? $elm$core$Maybe$Just(problem) : $elm$core$Maybe$Nothing;
	});
var $elm$browser$Debugger$Metadata$Decoder = {$: 'Decoder'};
var $elm$browser$Debugger$Metadata$Function = {$: 'Function'};
var $elm$browser$Debugger$Metadata$Process = {$: 'Process'};
var $elm$browser$Debugger$Metadata$Program = {$: 'Program'};
var $elm$browser$Debugger$Metadata$Request = {$: 'Request'};
var $elm$browser$Debugger$Metadata$Socket = {$: 'Socket'};
var $elm$browser$Debugger$Metadata$Task = {$: 'Task'};
var $elm$browser$Debugger$Metadata$VirtualDom = {$: 'VirtualDom'};
var $elm$browser$Debugger$Metadata$problemTable = _List_fromArray(
	[
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Function, '->'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Decoder, 'Json.Decode.Decoder'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Task, 'Task.Task'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Process, 'Process.Id'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Socket, 'WebSocket.LowLevel.WebSocket'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Request, 'Http.Request'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Program, 'Platform.Program'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, 'VirtualDom.Node'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, 'VirtualDom.Attribute')
	]);
var $elm$browser$Debugger$Metadata$findProblems = function (tipe) {
	return A2(
		$elm$core$List$filterMap,
		$elm$browser$Debugger$Metadata$hasProblem(tipe),
		$elm$browser$Debugger$Metadata$problemTable);
};
var $elm$browser$Debugger$Metadata$collectBadAliases = F3(
	function (name, _v0, list) {
		var tipe = _v0.tipe;
		var _v1 = $elm$browser$Debugger$Metadata$findProblems(tipe);
		if (!_v1.b) {
			return list;
		} else {
			var problems = _v1;
			return A2(
				$elm$core$List$cons,
				A2($elm$browser$Debugger$Metadata$ProblemType, name, problems),
				list);
		}
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm$browser$Debugger$Metadata$collectBadUnions = F3(
	function (name, _v0, list) {
		var tags = _v0.tags;
		var _v1 = A2(
			$elm$core$List$concatMap,
			$elm$browser$Debugger$Metadata$findProblems,
			$elm$core$List$concat(
				$elm$core$Dict$values(tags)));
		if (!_v1.b) {
			return list;
		} else {
			var problems = _v1;
			return A2(
				$elm$core$List$cons,
				A2($elm$browser$Debugger$Metadata$ProblemType, name, problems),
				list);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$browser$Debugger$Metadata$isPortable = function (_v0) {
	var types = _v0.types;
	var badAliases = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadAliases, _List_Nil, types.aliases);
	var _v1 = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadUnions, badAliases, types.unions);
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var problems = _v1;
		return $elm$core$Maybe$Just(
			A2($elm$browser$Debugger$Metadata$Error, types.message, problems));
	}
};
var $elm$browser$Debugger$Metadata$decode = function (value) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $elm$browser$Debugger$Metadata$decoder, value);
	if (_v0.$ === 'Err') {
		return $elm$core$Result$Err(
			A2($elm$browser$Debugger$Metadata$Error, 'The compiler is generating bad metadata. This is a compiler bug!', _List_Nil));
	} else {
		var metadata = _v0.a;
		var _v1 = $elm$browser$Debugger$Metadata$isPortable(metadata);
		if (_v1.$ === 'Nothing') {
			return $elm$core$Result$Ok(metadata);
		} else {
			var error = _v1.a;
			return $elm$core$Result$Err(error);
		}
	}
};
var $elm$browser$Debugger$History$History = F3(
	function (snapshots, recent, numMessages) {
		return {numMessages: numMessages, recent: recent, snapshots: snapshots};
	});
var $elm$browser$Debugger$History$RecentHistory = F3(
	function (model, messages, numMessages) {
		return {messages: messages, model: model, numMessages: numMessages};
	});
var $elm$browser$Debugger$History$empty = function (model) {
	return A3(
		$elm$browser$Debugger$History$History,
		$elm$core$Array$empty,
		A3($elm$browser$Debugger$History$RecentHistory, model, _List_Nil, 0),
		0);
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$browser$Debugger$Expando$initHelp = F2(
	function (isOuter, expando) {
		switch (expando.$) {
			case 'S':
				return expando;
			case 'Primitive':
				return expando;
			case 'Sequence':
				var seqType = expando.a;
				var isClosed = expando.b;
				var items = expando.c;
				return isOuter ? A3(
					$elm$browser$Debugger$Expando$Sequence,
					seqType,
					false,
					A2(
						$elm$core$List$map,
						$elm$browser$Debugger$Expando$initHelp(false),
						items)) : (($elm$core$List$length(items) <= 8) ? A3($elm$browser$Debugger$Expando$Sequence, seqType, false, items) : expando);
			case 'Dictionary':
				var isClosed = expando.a;
				var keyValuePairs = expando.b;
				return isOuter ? A2(
					$elm$browser$Debugger$Expando$Dictionary,
					false,
					A2(
						$elm$core$List$map,
						function (_v1) {
							var k = _v1.a;
							var v = _v1.b;
							return _Utils_Tuple2(
								k,
								A2($elm$browser$Debugger$Expando$initHelp, false, v));
						},
						keyValuePairs)) : (($elm$core$List$length(keyValuePairs) <= 8) ? A2($elm$browser$Debugger$Expando$Dictionary, false, keyValuePairs) : expando);
			case 'Record':
				var isClosed = expando.a;
				var entries = expando.b;
				return isOuter ? A2(
					$elm$browser$Debugger$Expando$Record,
					false,
					A2(
						$elm$core$Dict$map,
						F2(
							function (_v2, v) {
								return A2($elm$browser$Debugger$Expando$initHelp, false, v);
							}),
						entries)) : (($elm$core$Dict$size(entries) <= 4) ? A2($elm$browser$Debugger$Expando$Record, false, entries) : expando);
			default:
				var maybeName = expando.a;
				var isClosed = expando.b;
				var args = expando.c;
				return isOuter ? A3(
					$elm$browser$Debugger$Expando$Constructor,
					maybeName,
					false,
					A2(
						$elm$core$List$map,
						$elm$browser$Debugger$Expando$initHelp(false),
						args)) : (($elm$core$List$length(args) <= 4) ? A3($elm$browser$Debugger$Expando$Constructor, maybeName, false, args) : expando);
		}
	});
var $elm$browser$Debugger$Expando$init = function (value) {
	return A2(
		$elm$browser$Debugger$Expando$initHelp,
		true,
		_Debugger_init(value));
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$browser$Debugger$Overlay$None = {$: 'None'};
var $elm$browser$Debugger$Overlay$none = $elm$browser$Debugger$Overlay$None;
var $elm$browser$Debugger$Main$wrapInit = F4(
	function (metadata, popout, init, flags) {
		var _v0 = init(flags);
		var userModel = _v0.a;
		var userCommands = _v0.b;
		return _Utils_Tuple2(
			{
				expandoModel: $elm$browser$Debugger$Expando$init(userModel),
				expandoMsg: $elm$browser$Debugger$Expando$init(_Utils_Tuple0),
				history: $elm$browser$Debugger$History$empty(userModel),
				layout: A3($elm$browser$Debugger$Main$Horizontal, $elm$browser$Debugger$Main$Static, 0.3, 0.5),
				metadata: $elm$browser$Debugger$Metadata$decode(metadata),
				overlay: $elm$browser$Debugger$Overlay$none,
				popout: popout,
				state: $elm$browser$Debugger$Main$Running(userModel)
			},
			A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCommands));
	});
var $elm$browser$Debugger$Main$getLatestModel = function (state) {
	if (state.$ === 'Running') {
		var model = state.a;
		return model;
	} else {
		var model = state.c;
		return model;
	}
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Debugger$Main$wrapSubs = F2(
	function (subscriptions, model) {
		return A2(
			$elm$core$Platform$Sub$map,
			$elm$browser$Debugger$Main$UserMsg,
			subscriptions(
				$elm$browser$Debugger$Main$getLatestModel(model.state)));
	});
var $elm$browser$Debugger$Main$Moving = {$: 'Moving'};
var $elm$browser$Debugger$Main$Paused = F5(
	function (a, b, c, d, e) {
		return {$: 'Paused', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$browser$Debugger$History$Snapshot = F2(
	function (model, messages) {
		return {messages: messages, model: model};
	});
var $elm$browser$Debugger$History$addRecent = F3(
	function (msg, newModel, _v0) {
		var model = _v0.model;
		var messages = _v0.messages;
		var numMessages = _v0.numMessages;
		return _Utils_eq(numMessages, $elm$browser$Debugger$History$maxSnapshotSize) ? _Utils_Tuple2(
			$elm$core$Maybe$Just(
				A2(
					$elm$browser$Debugger$History$Snapshot,
					model,
					$elm$core$Array$fromList(messages))),
			A3(
				$elm$browser$Debugger$History$RecentHistory,
				newModel,
				_List_fromArray(
					[msg]),
				1)) : _Utils_Tuple2(
			$elm$core$Maybe$Nothing,
			A3(
				$elm$browser$Debugger$History$RecentHistory,
				model,
				A2($elm$core$List$cons, msg, messages),
				numMessages + 1));
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $elm$browser$Debugger$History$add = F3(
	function (msg, model, _v0) {
		var snapshots = _v0.snapshots;
		var recent = _v0.recent;
		var numMessages = _v0.numMessages;
		var _v1 = A3($elm$browser$Debugger$History$addRecent, msg, model, recent);
		if (_v1.a.$ === 'Just') {
			var snapshot = _v1.a.a;
			var newRecent = _v1.b;
			return A3(
				$elm$browser$Debugger$History$History,
				A2($elm$core$Array$push, snapshot, snapshots),
				newRecent,
				numMessages + 1);
		} else {
			var _v2 = _v1.a;
			var newRecent = _v1.b;
			return A3($elm$browser$Debugger$History$History, snapshots, newRecent, numMessages + 1);
		}
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$browser$Debugger$Overlay$BadImport = function (a) {
	return {$: 'BadImport', a: a};
};
var $elm$browser$Debugger$Overlay$RiskyImport = F2(
	function (a, b) {
		return {$: 'RiskyImport', a: a, b: b};
	});
var $elm$browser$Debugger$Report$VersionChanged = F2(
	function (a, b) {
		return {$: 'VersionChanged', a: a, b: b};
	});
var $elm$browser$Debugger$Report$MessageChanged = F2(
	function (a, b) {
		return {$: 'MessageChanged', a: a, b: b};
	});
var $elm$browser$Debugger$Report$SomethingChanged = function (a) {
	return {$: 'SomethingChanged', a: a};
};
var $elm$browser$Debugger$Report$AliasChange = function (a) {
	return {$: 'AliasChange', a: a};
};
var $elm$browser$Debugger$Metadata$checkAlias = F4(
	function (name, old, _new, changes) {
		return (_Utils_eq(old.tipe, _new.tipe) && _Utils_eq(old.args, _new.args)) ? changes : A2(
			$elm$core$List$cons,
			$elm$browser$Debugger$Report$AliasChange(name),
			changes);
	});
var $elm$browser$Debugger$Report$UnionChange = F2(
	function (a, b) {
		return {$: 'UnionChange', a: a, b: b};
	});
var $elm$browser$Debugger$Metadata$addTag = F3(
	function (tag, _v0, changes) {
		return _Utils_update(
			changes,
			{
				added: A2($elm$core$List$cons, tag, changes.added)
			});
	});
var $elm$browser$Debugger$Metadata$checkTag = F4(
	function (tag, old, _new, changes) {
		return _Utils_eq(old, _new) ? changes : _Utils_update(
			changes,
			{
				changed: A2($elm$core$List$cons, tag, changes.changed)
			});
	});
var $elm$browser$Debugger$Report$TagChanges = F4(
	function (removed, changed, added, argsMatch) {
		return {added: added, argsMatch: argsMatch, changed: changed, removed: removed};
	});
var $elm$browser$Debugger$Report$emptyTagChanges = function (argsMatch) {
	return A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, argsMatch);
};
var $elm$browser$Debugger$Report$hasTagChanges = function (tagChanges) {
	return _Utils_eq(
		tagChanges,
		A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, true));
};
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Debugger$Metadata$removeTag = F3(
	function (tag, _v0, changes) {
		return _Utils_update(
			changes,
			{
				removed: A2($elm$core$List$cons, tag, changes.removed)
			});
	});
var $elm$browser$Debugger$Metadata$checkUnion = F4(
	function (name, old, _new, changes) {
		var tagChanges = A6(
			$elm$core$Dict$merge,
			$elm$browser$Debugger$Metadata$removeTag,
			$elm$browser$Debugger$Metadata$checkTag,
			$elm$browser$Debugger$Metadata$addTag,
			old.tags,
			_new.tags,
			$elm$browser$Debugger$Report$emptyTagChanges(
				_Utils_eq(old.args, _new.args)));
		return $elm$browser$Debugger$Report$hasTagChanges(tagChanges) ? changes : A2(
			$elm$core$List$cons,
			A2($elm$browser$Debugger$Report$UnionChange, name, tagChanges),
			changes);
	});
var $elm$browser$Debugger$Metadata$ignore = F3(
	function (key, value, report) {
		return report;
	});
var $elm$browser$Debugger$Metadata$checkTypes = F2(
	function (old, _new) {
		return (!_Utils_eq(old.message, _new.message)) ? A2($elm$browser$Debugger$Report$MessageChanged, old.message, _new.message) : $elm$browser$Debugger$Report$SomethingChanged(
			A6(
				$elm$core$Dict$merge,
				$elm$browser$Debugger$Metadata$ignore,
				$elm$browser$Debugger$Metadata$checkUnion,
				$elm$browser$Debugger$Metadata$ignore,
				old.unions,
				_new.unions,
				A6($elm$core$Dict$merge, $elm$browser$Debugger$Metadata$ignore, $elm$browser$Debugger$Metadata$checkAlias, $elm$browser$Debugger$Metadata$ignore, old.aliases, _new.aliases, _List_Nil)));
	});
var $elm$browser$Debugger$Metadata$check = F2(
	function (old, _new) {
		return (!_Utils_eq(old.versions.elm, _new.versions.elm)) ? A2($elm$browser$Debugger$Report$VersionChanged, old.versions.elm, _new.versions.elm) : A2($elm$browser$Debugger$Metadata$checkTypes, old.types, _new.types);
	});
var $elm$browser$Debugger$Report$CorruptHistory = {$: 'CorruptHistory'};
var $elm$browser$Debugger$Overlay$corruptImport = $elm$browser$Debugger$Overlay$BadImport($elm$browser$Debugger$Report$CorruptHistory);
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$browser$Debugger$Report$Fine = {$: 'Fine'};
var $elm$browser$Debugger$Report$Impossible = {$: 'Impossible'};
var $elm$browser$Debugger$Report$Risky = {$: 'Risky'};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$browser$Debugger$Report$some = function (list) {
	return !$elm$core$List$isEmpty(list);
};
var $elm$browser$Debugger$Report$evaluateChange = function (change) {
	if (change.$ === 'AliasChange') {
		return $elm$browser$Debugger$Report$Impossible;
	} else {
		var removed = change.b.removed;
		var changed = change.b.changed;
		var added = change.b.added;
		var argsMatch = change.b.argsMatch;
		return ((!argsMatch) || ($elm$browser$Debugger$Report$some(changed) || $elm$browser$Debugger$Report$some(removed))) ? $elm$browser$Debugger$Report$Impossible : ($elm$browser$Debugger$Report$some(added) ? $elm$browser$Debugger$Report$Risky : $elm$browser$Debugger$Report$Fine);
	}
};
var $elm$browser$Debugger$Report$worstCase = F2(
	function (status, statusList) {
		worstCase:
		while (true) {
			if (!statusList.b) {
				return status;
			} else {
				switch (statusList.a.$) {
					case 'Impossible':
						var _v1 = statusList.a;
						return $elm$browser$Debugger$Report$Impossible;
					case 'Risky':
						var _v2 = statusList.a;
						var rest = statusList.b;
						var $temp$status = $elm$browser$Debugger$Report$Risky,
							$temp$statusList = rest;
						status = $temp$status;
						statusList = $temp$statusList;
						continue worstCase;
					default:
						var _v3 = statusList.a;
						var rest = statusList.b;
						var $temp$status = status,
							$temp$statusList = rest;
						status = $temp$status;
						statusList = $temp$statusList;
						continue worstCase;
				}
			}
		}
	});
var $elm$browser$Debugger$Report$evaluate = function (report) {
	switch (report.$) {
		case 'CorruptHistory':
			return $elm$browser$Debugger$Report$Impossible;
		case 'VersionChanged':
			return $elm$browser$Debugger$Report$Impossible;
		case 'MessageChanged':
			return $elm$browser$Debugger$Report$Impossible;
		default:
			var changes = report.a;
			return A2(
				$elm$browser$Debugger$Report$worstCase,
				$elm$browser$Debugger$Report$Fine,
				A2($elm$core$List$map, $elm$browser$Debugger$Report$evaluateChange, changes));
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $elm$browser$Debugger$Overlay$uploadDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (x, y) {
			return _Utils_Tuple2(x, y);
		}),
	A2($elm$json$Json$Decode$field, 'metadata', $elm$browser$Debugger$Metadata$decoder),
	A2($elm$json$Json$Decode$field, 'history', $elm$json$Json$Decode$value));
var $elm$browser$Debugger$Overlay$assessImport = F2(
	function (metadata, jsonString) {
		var _v0 = A2($elm$json$Json$Decode$decodeString, $elm$browser$Debugger$Overlay$uploadDecoder, jsonString);
		if (_v0.$ === 'Err') {
			return $elm$core$Result$Err($elm$browser$Debugger$Overlay$corruptImport);
		} else {
			var _v1 = _v0.a;
			var foreignMetadata = _v1.a;
			var rawHistory = _v1.b;
			var report = A2($elm$browser$Debugger$Metadata$check, foreignMetadata, metadata);
			var _v2 = $elm$browser$Debugger$Report$evaluate(report);
			switch (_v2.$) {
				case 'Impossible':
					return $elm$core$Result$Err(
						$elm$browser$Debugger$Overlay$BadImport(report));
				case 'Risky':
					return $elm$core$Result$Err(
						A2($elm$browser$Debugger$Overlay$RiskyImport, report, rawHistory));
				default:
					return $elm$core$Result$Ok(rawHistory);
			}
		}
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$browser$Debugger$Overlay$close = F2(
	function (msg, state) {
		switch (state.$) {
			case 'None':
				return $elm$core$Maybe$Nothing;
			case 'BadMetadata':
				return $elm$core$Maybe$Nothing;
			case 'BadImport':
				return $elm$core$Maybe$Nothing;
			default:
				var rawHistory = state.b;
				if (msg.$ === 'Cancel') {
					return $elm$core$Maybe$Nothing;
				} else {
					return $elm$core$Maybe$Just(rawHistory);
				}
		}
	});
var $elm$browser$Debugger$History$elmToJs = A2($elm$core$Basics$composeR, _Json_wrap, _Debugger_unsafeCoerce);
var $elm$browser$Debugger$History$encodeHelp = F2(
	function (snapshot, allMessages) {
		return A3($elm$core$Array$foldl, $elm$core$List$cons, allMessages, snapshot.messages);
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$browser$Debugger$History$encode = function (_v0) {
	var snapshots = _v0.snapshots;
	var recent = _v0.recent;
	return A2(
		$elm$json$Json$Encode$list,
		$elm$browser$Debugger$History$elmToJs,
		A3(
			$elm$core$Array$foldr,
			$elm$browser$Debugger$History$encodeHelp,
			$elm$core$List$reverse(recent.messages),
			snapshots));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$browser$Debugger$Metadata$encodeAlias = function (_v0) {
	var args = _v0.args;
	var tipe = _v0.tipe;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'args',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
				_Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(tipe))
			]));
};
var $elm$browser$Debugger$Metadata$encodeDict = F2(
	function (f, dict) {
		return $elm$json$Json$Encode$object(
			$elm$core$Dict$toList(
				A2(
					$elm$core$Dict$map,
					F2(
						function (key, value) {
							return f(value);
						}),
					dict)));
	});
var $elm$browser$Debugger$Metadata$encodeUnion = function (_v0) {
	var args = _v0.args;
	var tags = _v0.tags;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'args',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
				_Utils_Tuple2(
				'tags',
				A2(
					$elm$browser$Debugger$Metadata$encodeDict,
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
					tags))
			]));
};
var $elm$browser$Debugger$Metadata$encodeTypes = function (_v0) {
	var message = _v0.message;
	var unions = _v0.unions;
	var aliases = _v0.aliases;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'message',
				$elm$json$Json$Encode$string(message)),
				_Utils_Tuple2(
				'aliases',
				A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeAlias, aliases)),
				_Utils_Tuple2(
				'unions',
				A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeUnion, unions))
			]));
};
var $elm$browser$Debugger$Metadata$encodeVersions = function (_v0) {
	var elm = _v0.elm;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'elm',
				$elm$json$Json$Encode$string(elm))
			]));
};
var $elm$browser$Debugger$Metadata$encode = function (_v0) {
	var versions = _v0.versions;
	var types = _v0.types;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'versions',
				$elm$browser$Debugger$Metadata$encodeVersions(versions)),
				_Utils_Tuple2(
				'types',
				$elm$browser$Debugger$Metadata$encodeTypes(types))
			]));
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Debugger$Main$download = F2(
	function (metadata, history) {
		var historyLength = $elm$browser$Debugger$History$size(history);
		return A2(
			$elm$core$Task$perform,
			function (_v0) {
				return $elm$browser$Debugger$Main$NoOp;
			},
			A2(
				_Debugger_download,
				historyLength,
				_Json_unwrap(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'metadata',
								$elm$browser$Debugger$Metadata$encode(metadata)),
								_Utils_Tuple2(
								'history',
								$elm$browser$Debugger$History$encode(history))
							])))));
	});
var $elm$browser$Debugger$Main$Vertical = F3(
	function (a, b, c) {
		return {$: 'Vertical', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Main$drag = F2(
	function (info, layout) {
		if (layout.$ === 'Horizontal') {
			var status = layout.a;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Horizontal, status, info.x / info.width, y);
		} else {
			var status = layout.a;
			var x = layout.b;
			return A3($elm$browser$Debugger$Main$Vertical, status, x, info.y / info.height);
		}
	});
var $elm$browser$Debugger$History$Stepping = F2(
	function (a, b) {
		return {$: 'Stepping', a: a, b: b};
	});
var $elm$browser$Debugger$History$Done = F2(
	function (a, b) {
		return {$: 'Done', a: a, b: b};
	});
var $elm$browser$Debugger$History$getHelp = F3(
	function (update, msg, getResult) {
		if (getResult.$ === 'Done') {
			return getResult;
		} else {
			var n = getResult.a;
			var model = getResult.b;
			return (!n) ? A2(
				$elm$browser$Debugger$History$Done,
				msg,
				A2(update, msg, model).a) : A2(
				$elm$browser$Debugger$History$Stepping,
				n - 1,
				A2(update, msg, model).a);
		}
	});
var $elm$browser$Debugger$History$undone = function (getResult) {
	undone:
	while (true) {
		if (getResult.$ === 'Done') {
			var msg = getResult.a;
			var model = getResult.b;
			return _Utils_Tuple2(model, msg);
		} else {
			var $temp$getResult = getResult;
			getResult = $temp$getResult;
			continue undone;
		}
	}
};
var $elm$browser$Debugger$History$get = F3(
	function (update, index, history) {
		get:
		while (true) {
			var recent = history.recent;
			var snapshotMax = history.numMessages - recent.numMessages;
			if (_Utils_cmp(index, snapshotMax) > -1) {
				return $elm$browser$Debugger$History$undone(
					A3(
						$elm$core$List$foldr,
						$elm$browser$Debugger$History$getHelp(update),
						A2($elm$browser$Debugger$History$Stepping, index - snapshotMax, recent.model),
						recent.messages));
			} else {
				var _v0 = A2($elm$core$Array$get, (index / $elm$browser$Debugger$History$maxSnapshotSize) | 0, history.snapshots);
				if (_v0.$ === 'Nothing') {
					var $temp$update = update,
						$temp$index = index,
						$temp$history = history;
					update = $temp$update;
					index = $temp$index;
					history = $temp$history;
					continue get;
				} else {
					var model = _v0.a.model;
					var messages = _v0.a.messages;
					return $elm$browser$Debugger$History$undone(
						A3(
							$elm$core$Array$foldr,
							$elm$browser$Debugger$History$getHelp(update),
							A2($elm$browser$Debugger$History$Stepping, index % $elm$browser$Debugger$History$maxSnapshotSize, model),
							messages));
				}
			}
		}
	});
var $elm$browser$Debugger$History$getRecentMsg = function (history) {
	getRecentMsg:
	while (true) {
		var _v0 = history.recent.messages;
		if (!_v0.b) {
			var $temp$history = history;
			history = $temp$history;
			continue getRecentMsg;
		} else {
			var first = _v0.a;
			return first;
		}
	}
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$browser$Debugger$Expando$mergeDictHelp = F3(
	function (oldDict, key, value) {
		var _v12 = A2($elm$core$Dict$get, key, oldDict);
		if (_v12.$ === 'Nothing') {
			return value;
		} else {
			var oldValue = _v12.a;
			return A2($elm$browser$Debugger$Expando$mergeHelp, oldValue, value);
		}
	});
var $elm$browser$Debugger$Expando$mergeHelp = F2(
	function (old, _new) {
		var _v3 = _Utils_Tuple2(old, _new);
		_v3$6:
		while (true) {
			switch (_v3.b.$) {
				case 'S':
					return _new;
				case 'Primitive':
					return _new;
				case 'Sequence':
					if (_v3.a.$ === 'Sequence') {
						var _v4 = _v3.a;
						var isClosed = _v4.b;
						var oldValues = _v4.c;
						var _v5 = _v3.b;
						var seqType = _v5.a;
						var newValues = _v5.c;
						return A3(
							$elm$browser$Debugger$Expando$Sequence,
							seqType,
							isClosed,
							A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
					} else {
						break _v3$6;
					}
				case 'Dictionary':
					if (_v3.a.$ === 'Dictionary') {
						var _v6 = _v3.a;
						var isClosed = _v6.a;
						var _v7 = _v3.b;
						var keyValuePairs = _v7.b;
						return A2($elm$browser$Debugger$Expando$Dictionary, isClosed, keyValuePairs);
					} else {
						break _v3$6;
					}
				case 'Record':
					if (_v3.a.$ === 'Record') {
						var _v8 = _v3.a;
						var isClosed = _v8.a;
						var oldDict = _v8.b;
						var _v9 = _v3.b;
						var newDict = _v9.b;
						return A2(
							$elm$browser$Debugger$Expando$Record,
							isClosed,
							A2(
								$elm$core$Dict$map,
								$elm$browser$Debugger$Expando$mergeDictHelp(oldDict),
								newDict));
					} else {
						break _v3$6;
					}
				default:
					if (_v3.a.$ === 'Constructor') {
						var _v10 = _v3.a;
						var isClosed = _v10.b;
						var oldValues = _v10.c;
						var _v11 = _v3.b;
						var maybeName = _v11.a;
						var newValues = _v11.c;
						return A3(
							$elm$browser$Debugger$Expando$Constructor,
							maybeName,
							isClosed,
							A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
					} else {
						break _v3$6;
					}
			}
		}
		return _new;
	});
var $elm$browser$Debugger$Expando$mergeListHelp = F2(
	function (olds, news) {
		var _v0 = _Utils_Tuple2(olds, news);
		if (!_v0.a.b) {
			return news;
		} else {
			if (!_v0.b.b) {
				return news;
			} else {
				var _v1 = _v0.a;
				var x = _v1.a;
				var xs = _v1.b;
				var _v2 = _v0.b;
				var y = _v2.a;
				var ys = _v2.b;
				return A2(
					$elm$core$List$cons,
					A2($elm$browser$Debugger$Expando$mergeHelp, x, y),
					A2($elm$browser$Debugger$Expando$mergeListHelp, xs, ys));
			}
		}
	});
var $elm$browser$Debugger$Expando$merge = F2(
	function (value, expando) {
		return A2(
			$elm$browser$Debugger$Expando$mergeHelp,
			expando,
			_Debugger_init(value));
	});
var $elm$browser$Debugger$Main$jumpUpdate = F3(
	function (update, index, model) {
		var history = $elm$browser$Debugger$Main$cachedHistory(model);
		var currentMsg = $elm$browser$Debugger$History$getRecentMsg(history);
		var currentModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
		var _v0 = A3($elm$browser$Debugger$History$get, update, index, history);
		var indexModel = _v0.a;
		var indexMsg = _v0.b;
		return _Utils_update(
			model,
			{
				expandoModel: A2($elm$browser$Debugger$Expando$merge, indexModel, model.expandoModel),
				expandoMsg: A2($elm$browser$Debugger$Expando$merge, indexMsg, model.expandoMsg),
				state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, currentModel, currentMsg, history)
			});
	});
var $elm$browser$Debugger$History$jsToElm = A2($elm$core$Basics$composeR, _Json_unwrap, _Debugger_unsafeCoerce);
var $elm$browser$Debugger$History$decoder = F2(
	function (initialModel, update) {
		var addMessage = F2(
			function (rawMsg, _v0) {
				var model = _v0.a;
				var history = _v0.b;
				var msg = $elm$browser$Debugger$History$jsToElm(rawMsg);
				return _Utils_Tuple2(
					A2(update, msg, model),
					A3($elm$browser$Debugger$History$add, msg, model, history));
			});
		var updateModel = function (rawMsgs) {
			return A3(
				$elm$core$List$foldl,
				addMessage,
				_Utils_Tuple2(
					initialModel,
					$elm$browser$Debugger$History$empty(initialModel)),
				rawMsgs);
		};
		return A2(
			$elm$json$Json$Decode$map,
			updateModel,
			$elm$json$Json$Decode$list($elm$json$Json$Decode$value));
	});
var $elm$browser$Debugger$History$getInitialModel = function (_v0) {
	var snapshots = _v0.snapshots;
	var recent = _v0.recent;
	var _v1 = A2($elm$core$Array$get, 0, snapshots);
	if (_v1.$ === 'Just') {
		var model = _v1.a.model;
		return model;
	} else {
		return recent.model;
	}
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$browser$Debugger$Main$loadNewHistory = F3(
	function (rawHistory, update, model) {
		var pureUserUpdate = F2(
			function (msg, userModel) {
				return A2(update, msg, userModel).a;
			});
		var initialUserModel = $elm$browser$Debugger$History$getInitialModel(model.history);
		var decoder = A2($elm$browser$Debugger$History$decoder, initialUserModel, pureUserUpdate);
		var _v0 = A2($elm$json$Json$Decode$decodeValue, decoder, rawHistory);
		if (_v0.$ === 'Err') {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{overlay: $elm$browser$Debugger$Overlay$corruptImport}),
				$elm$core$Platform$Cmd$none);
		} else {
			var _v1 = _v0.a;
			var latestUserModel = _v1.a;
			var newHistory = _v1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						expandoModel: $elm$browser$Debugger$Expando$init(latestUserModel),
						expandoMsg: $elm$browser$Debugger$Expando$init(
							$elm$browser$Debugger$History$getRecentMsg(newHistory)),
						history: newHistory,
						overlay: $elm$browser$Debugger$Overlay$none,
						state: $elm$browser$Debugger$Main$Running(latestUserModel)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $elm$browser$Debugger$Main$scroll = function (popout) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
		_Debugger_scroll(popout));
};
var $elm$browser$Debugger$Main$scrollTo = F2(
	function (id, popout) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
			A2(_Debugger_scrollTo, id, popout));
	});
var $elm$browser$Debugger$Main$setDragStatus = F2(
	function (status, layout) {
		if (layout.$ === 'Horizontal') {
			var x = layout.b;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Horizontal, status, x, y);
		} else {
			var x = layout.b;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Vertical, status, x, y);
		}
	});
var $elm$browser$Debugger$Main$swapLayout = function (layout) {
	if (layout.$ === 'Horizontal') {
		var s = layout.a;
		var x = layout.b;
		var y = layout.c;
		return A3($elm$browser$Debugger$Main$Vertical, s, x, y);
	} else {
		var s = layout.a;
		var x = layout.b;
		var y = layout.c;
		return A3($elm$browser$Debugger$Main$Horizontal, s, x, y);
	}
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$browser$Debugger$Expando$updateIndex = F3(
	function (n, func, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			var x = list.a;
			var xs = list.b;
			return (n <= 0) ? A2(
				$elm$core$List$cons,
				func(x),
				xs) : A2(
				$elm$core$List$cons,
				x,
				A3($elm$browser$Debugger$Expando$updateIndex, n - 1, func, xs));
		}
	});
var $elm$browser$Debugger$Expando$update = F2(
	function (msg, value) {
		switch (value.$) {
			case 'S':
				return value;
			case 'Primitive':
				return value;
			case 'Sequence':
				var seqType = value.a;
				var isClosed = value.b;
				var valueList = value.c;
				switch (msg.$) {
					case 'Toggle':
						return A3($elm$browser$Debugger$Expando$Sequence, seqType, !isClosed, valueList);
					case 'Index':
						if (msg.a.$ === 'None') {
							var _v3 = msg.a;
							var index = msg.b;
							var subMsg = msg.c;
							return A3(
								$elm$browser$Debugger$Expando$Sequence,
								seqType,
								isClosed,
								A3(
									$elm$browser$Debugger$Expando$updateIndex,
									index,
									$elm$browser$Debugger$Expando$update(subMsg),
									valueList));
						} else {
							return value;
						}
					default:
						return value;
				}
			case 'Dictionary':
				var isClosed = value.a;
				var keyValuePairs = value.b;
				switch (msg.$) {
					case 'Toggle':
						return A2($elm$browser$Debugger$Expando$Dictionary, !isClosed, keyValuePairs);
					case 'Index':
						var redirect = msg.a;
						var index = msg.b;
						var subMsg = msg.c;
						switch (redirect.$) {
							case 'None':
								return value;
							case 'Key':
								return A2(
									$elm$browser$Debugger$Expando$Dictionary,
									isClosed,
									A3(
										$elm$browser$Debugger$Expando$updateIndex,
										index,
										function (_v6) {
											var k = _v6.a;
											var v = _v6.b;
											return _Utils_Tuple2(
												A2($elm$browser$Debugger$Expando$update, subMsg, k),
												v);
										},
										keyValuePairs));
							default:
								return A2(
									$elm$browser$Debugger$Expando$Dictionary,
									isClosed,
									A3(
										$elm$browser$Debugger$Expando$updateIndex,
										index,
										function (_v7) {
											var k = _v7.a;
											var v = _v7.b;
											return _Utils_Tuple2(
												k,
												A2($elm$browser$Debugger$Expando$update, subMsg, v));
										},
										keyValuePairs));
						}
					default:
						return value;
				}
			case 'Record':
				var isClosed = value.a;
				var valueDict = value.b;
				switch (msg.$) {
					case 'Toggle':
						return A2($elm$browser$Debugger$Expando$Record, !isClosed, valueDict);
					case 'Index':
						return value;
					default:
						var field = msg.a;
						var subMsg = msg.b;
						return A2(
							$elm$browser$Debugger$Expando$Record,
							isClosed,
							A3(
								$elm$core$Dict$update,
								field,
								$elm$browser$Debugger$Expando$updateField(subMsg),
								valueDict));
				}
			default:
				var maybeName = value.a;
				var isClosed = value.b;
				var valueList = value.c;
				switch (msg.$) {
					case 'Toggle':
						return A3($elm$browser$Debugger$Expando$Constructor, maybeName, !isClosed, valueList);
					case 'Index':
						if (msg.a.$ === 'None') {
							var _v10 = msg.a;
							var index = msg.b;
							var subMsg = msg.c;
							return A3(
								$elm$browser$Debugger$Expando$Constructor,
								maybeName,
								isClosed,
								A3(
									$elm$browser$Debugger$Expando$updateIndex,
									index,
									$elm$browser$Debugger$Expando$update(subMsg),
									valueList));
						} else {
							return value;
						}
					default:
						return value;
				}
		}
	});
var $elm$browser$Debugger$Expando$updateField = F2(
	function (msg, maybeExpando) {
		if (maybeExpando.$ === 'Nothing') {
			return maybeExpando;
		} else {
			var expando = maybeExpando.a;
			return $elm$core$Maybe$Just(
				A2($elm$browser$Debugger$Expando$update, msg, expando));
		}
	});
var $elm$browser$Debugger$Main$Upload = function (a) {
	return {$: 'Upload', a: a};
};
var $elm$browser$Debugger$Main$upload = function (popout) {
	return A2(
		$elm$core$Task$perform,
		$elm$browser$Debugger$Main$Upload,
		_Debugger_upload(popout));
};
var $elm$browser$Debugger$Overlay$BadMetadata = function (a) {
	return {$: 'BadMetadata', a: a};
};
var $elm$browser$Debugger$Overlay$badMetadata = $elm$browser$Debugger$Overlay$BadMetadata;
var $elm$browser$Debugger$Main$withGoodMetadata = F2(
	function (model, func) {
		var _v0 = model.metadata;
		if (_v0.$ === 'Ok') {
			var metadata = _v0.a;
			return func(metadata);
		} else {
			var error = _v0.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						overlay: $elm$browser$Debugger$Overlay$badMetadata(error)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $elm$browser$Debugger$Main$wrapUpdate = F3(
	function (update, msg, model) {
		wrapUpdate:
		while (true) {
			switch (msg.$) {
				case 'NoOp':
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				case 'UserMsg':
					var userMsg = msg.a;
					var userModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
					var newHistory = A3($elm$browser$Debugger$History$add, userMsg, userModel, model.history);
					var _v1 = A2(update, userMsg, userModel);
					var newUserModel = _v1.a;
					var userCmds = _v1.b;
					var commands = A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCmds);
					var _v2 = model.state;
					if (_v2.$ === 'Running') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									expandoModel: A2($elm$browser$Debugger$Expando$merge, newUserModel, model.expandoModel),
									expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
									history: newHistory,
									state: $elm$browser$Debugger$Main$Running(newUserModel)
								}),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										commands,
										$elm$browser$Debugger$Main$scroll(model.popout)
									])));
					} else {
						var index = _v2.a;
						var indexModel = _v2.b;
						var history = _v2.e;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									history: newHistory,
									state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, newUserModel, userMsg, history)
								}),
							commands);
					}
				case 'TweakExpandoMsg':
					var eMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								expandoMsg: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoMsg)
							}),
						$elm$core$Platform$Cmd$none);
				case 'TweakExpandoModel':
					var eMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								expandoModel: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoModel)
							}),
						$elm$core$Platform$Cmd$none);
				case 'Resume':
					var _v3 = model.state;
					if (_v3.$ === 'Running') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var userModel = _v3.c;
						var userMsg = _v3.d;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									expandoModel: A2($elm$browser$Debugger$Expando$merge, userModel, model.expandoModel),
									expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
									state: $elm$browser$Debugger$Main$Running(userModel)
								}),
							$elm$browser$Debugger$Main$scroll(model.popout));
					}
				case 'Jump':
					var index = msg.a;
					return _Utils_Tuple2(
						A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model),
						$elm$core$Platform$Cmd$none);
				case 'SliderJump':
					var index = msg.a;
					return _Utils_Tuple2(
						A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model),
						A2(
							$elm$browser$Debugger$Main$scrollTo,
							$elm$browser$Debugger$History$idForMessageIndex(index),
							model.popout));
				case 'Open':
					return _Utils_Tuple2(
						model,
						A2(
							$elm$core$Task$perform,
							$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
							_Debugger_open(model.popout)));
				case 'Up':
					var _v4 = model.state;
					if (_v4.$ === 'Running') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var i = _v4.a;
						var history = _v4.e;
						var targetIndex = i + 1;
						if (_Utils_cmp(
							targetIndex,
							$elm$browser$Debugger$History$size(history)) < 0) {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$SliderJump(targetIndex),
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						} else {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$Resume,
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						}
					}
				case 'Down':
					var _v5 = model.state;
					if (_v5.$ === 'Running') {
						var $temp$update = update,
							$temp$msg = $elm$browser$Debugger$Main$Jump(
							$elm$browser$Debugger$History$size(model.history) - 1),
							$temp$model = model;
						update = $temp$update;
						msg = $temp$msg;
						model = $temp$model;
						continue wrapUpdate;
					} else {
						var index = _v5.a;
						if (index > 0) {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$SliderJump(index - 1),
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					}
				case 'Import':
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (_v6) {
							return _Utils_Tuple2(
								model,
								$elm$browser$Debugger$Main$upload(model.popout));
						});
				case 'Export':
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (metadata) {
							return _Utils_Tuple2(
								model,
								A2($elm$browser$Debugger$Main$download, metadata, model.history));
						});
				case 'Upload':
					var jsonString = msg.a;
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (metadata) {
							var _v7 = A2($elm$browser$Debugger$Overlay$assessImport, metadata, jsonString);
							if (_v7.$ === 'Err') {
								var newOverlay = _v7.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{overlay: newOverlay}),
									$elm$core$Platform$Cmd$none);
							} else {
								var rawHistory = _v7.a;
								return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
							}
						});
				case 'OverlayMsg':
					var overlayMsg = msg.a;
					var _v8 = A2($elm$browser$Debugger$Overlay$close, overlayMsg, model.overlay);
					if (_v8.$ === 'Nothing') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{overlay: $elm$browser$Debugger$Overlay$none}),
							$elm$core$Platform$Cmd$none);
					} else {
						var rawHistory = _v8.a;
						return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
					}
				case 'SwapLayout':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: $elm$browser$Debugger$Main$swapLayout(model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				case 'DragStart':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Moving, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				case 'Drag':
					var info = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$drag, info, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				default:
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Static, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$NotFound = {$: 'NotFound'};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Ports$getUser = _Platform_outgoingPort(
	'getUser',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Main$EtcMsg = function (a) {
	return {$: 'EtcMsg', a: a};
};
var $author$project$Main$EtcPage = function (a) {
	return {$: 'EtcPage', a: a};
};
var $author$project$Main$PostLongWordMsg = function (a) {
	return {$: 'PostLongWordMsg', a: a};
};
var $author$project$Main$PostLongWordPage = function (a) {
	return {$: 'PostLongWordPage', a: a};
};
var $author$project$Main$TypeAnyMsg = function (a) {
	return {$: 'TypeAnyMsg', a: a};
};
var $author$project$Main$TypeAnyPage = function (a) {
	return {$: 'TypeAnyPage', a: a};
};
var $author$project$Main$TypeLongWordMsg = function (a) {
	return {$: 'TypeLongWordMsg', a: a};
};
var $author$project$Main$TypeLongWordPage = function (a) {
	return {$: 'TypeLongWordPage', a: a};
};
var $author$project$Main$TypeShortWordMsg = function (a) {
	return {$: 'TypeShortWordMsg', a: a};
};
var $author$project$Main$TypeShortWordPage = function (a) {
	return {$: 'TypeShortWordPage', a: a};
};
var $author$project$Main$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var $author$project$Main$UserPage = function (a) {
	return {$: 'UserPage', a: a};
};
var $author$project$Main$WordListMsg = function (a) {
	return {$: 'WordListMsg', a: a};
};
var $author$project$Main$WordListPage = function (a) {
	return {$: 'WordListPage', a: a};
};
var $author$project$Page$Etc$Init = {$: 'Init'};
var $author$project$Page$Etc$Model = function (state) {
	return {state: state};
};
var $author$project$Page$Etc$init = function (env) {
	return _Utils_Tuple2(
		$author$project$Page$Etc$Model($author$project$Page$Etc$Init),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Page$PostLongWord$Edit = function (a) {
	return {$: 'Edit', a: a};
};
var $author$project$Page$PostLongWord$Init = {$: 'Init'};
var $author$project$Page$PostLongWord$Model = F2(
	function (state, lw) {
		return {lw: lw, state: state};
	});
var $author$project$Data$LongWord$LongWord = F4(
	function (title, id, wordForView, wordForInput) {
		return {id: id, title: title, wordForInput: wordForInput, wordForView: wordForView};
	});
var $author$project$Data$LongWord$new = A4($author$project$Data$LongWord$LongWord, '', 0, '', '');
var $author$project$Page$PostLongWord$init = function (env) {
	return _Utils_Tuple2(
		A2(
			$author$project$Page$PostLongWord$Model,
			$author$project$Page$PostLongWord$Edit($author$project$Page$PostLongWord$Init),
			$author$project$Data$LongWord$new),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Page$TypeAny$Model = F4(
	function (input, bestKPM, nowKPM, lastTime) {
		return {bestKPM: bestKPM, input: input, lastTime: lastTime, nowKPM: nowKPM};
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$Page$TypeAny$init = function (env) {
	return _Utils_Tuple2(
		A4(
			$author$project$Page$TypeAny$Model,
			'',
			0,
			0,
			$elm$time$Time$millisToPosix(0)),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Page$TypeLongWord$Init = {$: 'Init'};
var $author$project$Page$TypeLongWord$Model = function (modelState) {
	return function (typingState) {
		return function (inputHistory) {
			return function (typingData) {
				return function (wordForView) {
					return function (miss) {
						return function (missed) {
							return function (startTime) {
								return function (finishTime) {
									return function (bestScore) {
										return function (scoreHistory) {
											return function (notice) {
												return function (ranking) {
													return {bestScore: bestScore, finishTime: finishTime, inputHistory: inputHistory, miss: miss, missed: missed, modelState: modelState, notice: notice, ranking: ranking, scoreHistory: scoreHistory, startTime: startTime, typingData: typingData, typingState: typingState, wordForView: wordForView};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Page$TypeLongWord$ReceiveLongWord = function (a) {
	return {$: 'ReceiveLongWord', a: a};
};
var $author$project$Page$TypeLongWord$Waiting = {$: 'Waiting'};
var $author$project$API$api = '/TypingClubAPI.php';
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$API$encodeForGetLongWord = function (id) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(id))
			]));
};
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $author$project$API$host = 'https://pokosuko.work/typing1';
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Data$LongWord$longWordDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Data$LongWord$LongWord,
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'wordForView', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'wordForInput', $elm$json$Json$Decode$string));
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$post = function (r) {
	return $elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$API$getLongWord = F2(
	function (msg, id) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					$author$project$API$encodeForGetLongWord(id)),
				expect: A2($elm$http$Http$expectJson, msg, $author$project$Data$LongWord$longWordDecoder),
				url: $author$project$API$host + ($author$project$API$api + '?getLongWord')
			});
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$API$getPresentLongWord = function (msg) {
	return $elm$http$Http$get(
		{
			expect: A2($elm$http$Http$expectJson, msg, $author$project$Data$LongWord$longWordDecoder),
			url: $author$project$API$host + ($author$project$API$api + '?getPresentLongWord')
		});
};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Page$TypeLongWord$Top = function (a) {
	return {$: 'Top', a: a};
};
var $elm$url$Url$Parser$Internal$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$Query$custom = F2(
	function (key, func) {
		return $elm$url$Url$Parser$Internal$Parser(
			function (dict) {
				return func(
					A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						A2($elm$core$Dict$get, key, dict)));
			});
	});
var $elm$url$Url$Parser$Query$int = function (key) {
	return A2(
		$elm$url$Url$Parser$Query$custom,
		key,
		function (stringList) {
			if (stringList.b && (!stringList.b.b)) {
				var str = stringList.a;
				return $elm$core$String$toInt(str);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		});
};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$query = function (_v0) {
	var queryParser = _v0.a;
	return $elm$url$Url$Parser$Parser(
		function (_v1) {
			var visited = _v1.visited;
			var unvisited = _v1.unvisited;
			var params = _v1.params;
			var frag = _v1.frag;
			var value = _v1.value;
			return _List_fromArray(
				[
					A5(
					$elm$url$Url$Parser$State,
					visited,
					unvisited,
					params,
					frag,
					value(
						queryParser(params)))
				]);
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$questionMark = F2(
	function (parser, queryParser) {
		return A2(
			$elm$url$Url$Parser$slash,
			parser,
			$elm$url$Url$Parser$query(queryParser));
	});
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Page$TypeLongWord$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Page$TypeLongWord$Top,
			A2(
				$elm$url$Url$Parser$questionMark,
				$elm$url$Url$Parser$top,
				$elm$url$Url$Parser$Query$int('id')))
		]));
var $author$project$Page$TypeLongWord$init = function (env) {
	var url = env.url;
	var id = function () {
		var _v1 = A2(
			$elm$url$Url$Parser$parse,
			$author$project$Page$TypeLongWord$routeParser,
			_Utils_update(
				url,
				{path: '/'}));
		if (_v1.$ === 'Just') {
			var d1 = _v1.a.a;
			return d1;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}();
	return _Utils_Tuple2(
		$author$project$Page$TypeLongWord$Model($author$project$Page$TypeLongWord$Init)($author$project$Page$TypeLongWord$Waiting)('')($elm$core$Maybe$Nothing)('')(0)(false)(
			$elm$time$Time$millisToPosix(0))(
			$elm$time$Time$millisToPosix(0))($elm$core$Maybe$Nothing)(_List_Nil)('')($elm$core$Maybe$Nothing),
		function () {
			if (id.$ === 'Just') {
				var wid = id.a;
				return A2($author$project$API$getLongWord, $author$project$Page$TypeLongWord$ReceiveLongWord, wid);
			} else {
				return $author$project$API$getPresentLongWord($author$project$Page$TypeLongWord$ReceiveLongWord);
			}
		}());
};
var $author$project$Page$TypeShortWord$Model = F2(
	function (state, notice) {
		return {notice: notice, state: state};
	});
var $author$project$Page$TypeShortWord$Ready = function (a) {
	return {$: 'Ready', a: a};
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Page$TypeShortWord$ReadyData = F7(
	function (state, shortWords, customTypingWords, countDownTimer, bestScore, scoreHistory, ranking) {
		return {bestScore: bestScore, countDownTimer: countDownTimer, customTypingWords: customTypingWords, ranking: ranking, scoreHistory: scoreHistory, shortWords: shortWords, state: state};
	});
var $author$project$Page$TypeShortWord$Trial = {$: 'Trial'};
var $GoldentTuft$elm_japanese_typing$Typing$PrintRule = F3(
	function (input, output, priority) {
		return {input: input, output: output, priority: priority};
	});
var $GoldentTuft$elm_japanese_typing$Typing$getPower = function (max) {
	var f = F2(
		function (m, p) {
			f:
			while (true) {
				if (_Utils_cmp(m, p) < 0) {
					return p;
				} else {
					if (m > 10000000) {
						return 1;
					} else {
						var $temp$m = m,
							$temp$p = p * 10;
						m = $temp$m;
						p = $temp$p;
						continue f;
					}
				}
			}
		});
	return A2(f, max, 10);
};
var $GoldentTuft$elm_japanese_typing$Typing$getPriority = F2(
	function (rule, prs) {
		getPriority:
		while (true) {
			if (!prs.b) {
				return rule.priority;
			} else {
				var x = prs.a;
				var xs = prs.b;
				if (_Utils_eq(rule.input, x.input) && _Utils_eq(rule.output, x.output)) {
					return x.priority;
				} else {
					var $temp$rule = rule,
						$temp$prs = xs;
					rule = $temp$rule;
					prs = $temp$prs;
					continue getPriority;
				}
			}
		}
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities = F2(
	function (priorities, rules) {
		var ps = A2(
			$elm$core$List$map,
			function (p) {
				return p.priority;
			},
			priorities);
		var max = A2(
			$elm$core$Maybe$withDefault,
			1,
			$elm$core$List$maximum(ps));
		var power = $GoldentTuft$elm_japanese_typing$Typing$getPower(max);
		return A2(
			$elm$core$List$map,
			function (rule) {
				return _Utils_update(
					rule,
					{
						priority: (rule.priority * power) + A2($GoldentTuft$elm_japanese_typing$Typing$getPriority, rule, priorities)
					});
			},
			rules);
	});
var $GoldentTuft$elm_japanese_typing$Typing$Rule = F3(
	function (input, output, priority) {
		return {input: input, output: output, priority: priority};
	});
var $GoldentTuft$elm_japanese_typing$Typing$romanTable = _List_fromArray(
	[
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '-', 'ー', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '~', '〜', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '.', '。', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ',', '、', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '[', '「', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ']', '」', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'va', 'ゔぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vi', 'ゔぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vu', 'ゔ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 've', 'ゔぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vo', 'ゔぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vya', 'ゔゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vyi', 'ゔぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vyu', 'ゔゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vye', 'ゔぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vyo', 'ゔょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kya', 'きゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kyi', 'きぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kyu', 'きゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kye', 'きぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kyo', 'きょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gya', 'ぎゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gyi', 'ぎぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gyu', 'ぎゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gye', 'ぎぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gyo', 'ぎょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sya', 'しゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'syi', 'しぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'syu', 'しゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sye', 'しぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'syo', 'しょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sha', 'しゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'shi', 'し', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'shu', 'しゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'she', 'しぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sho', 'しょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zya', 'じゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zyi', 'じぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zyu', 'じゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zye', 'じぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zyo', 'じょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tya', 'ちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tyi', 'ちぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tyu', 'ちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tye', 'ちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tyo', 'ちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cha', 'ちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'chi', 'ち', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'chu', 'ちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'che', 'ちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cho', 'ちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cya', 'ちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cyi', 'ちぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cyu', 'ちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cye', 'ちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cyo', 'ちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dya', 'ぢゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dyi', 'ぢぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dyu', 'ぢゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dye', 'ぢぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dyo', 'ぢょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tsa', 'つぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tsi', 'つぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tse', 'つぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tso', 'つぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tha', 'てゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'thi', 'てぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'thu', 'てゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'the', 'てぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tho', 'てょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dha', 'でゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dhi', 'でぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dhu', 'でゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dhe', 'でぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dho', 'でょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'twa', 'とぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'twi', 'とぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'twu', 'とぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'twe', 'とぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'two', 'とぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dwa', 'どぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dwi', 'どぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dwu', 'どぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dwe', 'どぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dwo', 'どぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nya', 'にゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nyi', 'にぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nyu', 'にゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nye', 'にぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nyo', 'にょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hya', 'ひゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hyi', 'ひぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hyu', 'ひゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hye', 'ひぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hyo', 'ひょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bya', 'びゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'byi', 'びぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'byu', 'びゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bye', 'びぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'byo', 'びょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pya', 'ぴゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pyi', 'ぴぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pyu', 'ぴゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pye', 'ぴぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pyo', 'ぴょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fa', 'ふぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fi', 'ふぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fe', 'ふぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fo', 'ふぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fya', 'ふゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fyu', 'ふゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fyo', 'ふょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mya', 'みゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'myi', 'みぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'myu', 'みゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mye', 'みぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'myo', 'みょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rya', 'りゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ryi', 'りぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ryu', 'りゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rye', 'りぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ryo', 'りょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nn', 'ん', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'n', 'ん', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xn', 'ん', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'a', 'あ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'i', 'い', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'u', 'う', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wu', 'う', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'e', 'え', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'o', 'お', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xa', 'ぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xi', 'ぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xu', 'ぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xe', 'ぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xo', 'ぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'la', 'ぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'li', 'ぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lu', 'ぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'le', 'ぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lo', 'ぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lyi', 'ぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xyi', 'ぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lye', 'ぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xye', 'ぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ye', 'いぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ka', 'か', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ki', 'き', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ku', 'く', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ke', 'け', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ko', 'こ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xka', 'ヵ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xke', 'ヶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lka', 'ヵ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lke', 'ヶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ga', 'が', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gi', 'ぎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gu', 'ぐ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ge', 'げ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'go', 'ご', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sa', 'さ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'si', 'し', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'su', 'す', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'se', 'せ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'so', 'そ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ca', 'か', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ci', 'し', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cu', 'く', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ce', 'せ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'co', 'こ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qa', 'くぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qi', 'くぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qu', 'く', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qe', 'くぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qo', 'くぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kwa', 'くぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gwa', 'ぐぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gwi', 'ぐぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gwu', 'ぐぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gwe', 'ぐぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gwo', 'ぐぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'za', 'ざ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zi', 'じ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zu', 'ず', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ze', 'ぜ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zo', 'ぞ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ja', 'じゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ji', 'じ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ju', 'じゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'je', 'じぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jo', 'じょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jya', 'じゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jyi', 'じぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jyu', 'じゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jye', 'じぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jyo', 'じょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ta', 'た', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ti', 'ち', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tu', 'つ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tsu', 'つ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'te', 'て', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'to', 'と', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'da', 'だ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'di', 'ぢ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'du', 'づ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'de', 'で', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'do', 'ど', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xtu', 'っ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xtsu', 'っ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ltu', 'っ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ltsu', 'っ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'na', 'な', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ni', 'に', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'nu', 'ぬ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ne', 'ね', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'no', 'の', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ha', 'は', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hi', 'ひ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hu', 'ふ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'fu', 'ふ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'he', 'へ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ho', 'ほ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ba', 'ば', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bi', 'び', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bu', 'ぶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'be', 'べ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bo', 'ぼ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pa', 'ぱ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pi', 'ぴ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pu', 'ぷ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'pe', 'ぺ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'po', 'ぽ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ma', 'ま', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mi', 'み', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mu', 'む', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'me', 'め', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mo', 'も', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xya', 'ゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lya', 'ゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ya', 'や', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wyi', 'ゐ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xyu', 'ゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lyu', 'ゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yu', 'ゆ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wye', 'ゑ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xyo', 'ょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lyo', 'ょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yo', 'よ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ra', 'ら', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ri', 'り', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ru', 'る', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 're', 'れ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ro', 'ろ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xwa', 'ゎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lwa', 'ゎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wa', 'わ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wi', 'うぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'we', 'うぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wo', 'を', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wha', 'うぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'whi', 'うぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'whu', 'う', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'whe', 'うぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'who', 'うぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qqa', 'っくぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qqi', 'っくぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qqu', 'っく', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qqe', 'っくぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'qqo', 'っくぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vva', 'っゔぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvi', 'っゔぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvu', 'っゔ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vve', 'っゔぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvo', 'っゔぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvya', 'っゔゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvyi', 'っゔぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvyu', 'っゔゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvye', 'っゔぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'vvyo', 'っゔょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lla', 'っぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lli', 'っぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llu', 'っぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lle', 'っぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llo', 'っぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llyi', 'っぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llye', 'っぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llka', 'っヵ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llke', 'っヶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'lltu', 'っっ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llya', 'っゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llyu', 'っゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llyo', 'っょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'llwa', 'っゎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxn', 'っん', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxa', 'っぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxi', 'っぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxu', 'っぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxe', 'っぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxo', 'っぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxyi', 'っぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxye', 'っぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxka', 'っヵ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxke', 'っヶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxtu', 'っっ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxya', 'っゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxyu', 'っゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxyo', 'っょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'xxwa', 'っゎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkya', 'っきゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkyi', 'っきぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkyu', 'っきゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkye', 'っきぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkyo', 'っきょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kka', 'っか', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kki', 'っき', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kku', 'っく', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kke', 'っけ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kko', 'っこ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'kkwa', 'っくぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggya', 'っぎゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggyi', 'っぎぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggyu', 'っぎゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggye', 'っぎぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggyo', 'っぎょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gga', 'っが', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggi', 'っぎ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggu', 'っぐ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'gge', 'っげ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggo', 'っご', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggwa', 'っぐぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggwi', 'っぐぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggwu', 'っぐぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggwe', 'っぐぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ggwo', 'っぐぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssya', 'っしゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssyi', 'っしぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssyu', 'っしゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssye', 'っしぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssyo', 'っしょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssha', 'っしゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sshi', 'っし', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sshu', 'っしゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sshe', 'っしぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssho', 'っしょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssa', 'っさ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssi', 'っし', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ssu', 'っす', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sse', 'っせ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'sso', 'っそ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzya', 'っじゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzyi', 'っじぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzyu', 'っじゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzye', 'っじぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzyo', 'っじょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zza', 'っざ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzi', 'っじ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzu', 'っず', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zze', 'っぜ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'zzo', 'っぞ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jja', 'っじゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jji', 'っじ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jju', 'っじゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jje', 'っじぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjo', 'っじょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjya', 'っじゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjyi', 'っじぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjyu', 'っじゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjye', 'っじぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'jjyo', 'っじょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttya', 'っちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttyi', 'っちぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttyu', 'っちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttye', 'っちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttyo', 'っちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttsa', 'っつぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttsi', 'っつぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttse', 'っつぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttso', 'っつぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttha', 'ってゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tthi', 'ってぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tthu', 'ってゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tthe', 'ってぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttho', 'ってょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttwa', 'っとぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttwi', 'っとぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttwu', 'っとぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttwe', 'っとぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttwo', 'っとぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tta', 'った', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tti', 'っち', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttu', 'っつ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ttsu', 'っつ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tte', 'って', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'tto', 'っと', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddya', 'っぢゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddyi', 'っぢぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddyu', 'っぢゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddye', 'っぢぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddyo', 'っぢょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddha', 'っでゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddhi', 'っでぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddhu', 'っでゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddhe', 'っでぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddho', 'っでょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddwa', 'っどぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddwi', 'っどぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddwu', 'っどぅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddwe', 'っどぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddwo', 'っどぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dda', 'っだ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddi', 'っぢ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddu', 'っづ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'dde', 'っで', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ddo', 'っど', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhya', 'っひゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhyi', 'っひぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhyu', 'っひゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhye', 'っひぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhyo', 'っひょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hha', 'っは', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhi', 'っひ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhu', 'っふ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hhe', 'っへ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'hho', 'っほ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffa', 'っふぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffi', 'っふぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffe', 'っふぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffo', 'っふぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffya', 'っふゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffyu', 'っふゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffyo', 'っふょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ffu', 'っふ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbya', 'っびゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbyi', 'っびぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbyu', 'っびゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbye', 'っびぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbyo', 'っびょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bba', 'っば', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbi', 'っび', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbu', 'っぶ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbe', 'っべ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'bbo', 'っぼ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppya', 'っぴゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppyi', 'っぴぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppyu', 'っぴゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppye', 'っぴぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppyo', 'っぴょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppa', 'っぱ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppi', 'っぴ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppu', 'っぷ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppe', 'っぺ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ppo', 'っぽ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmya', 'っみゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmyi', 'っみぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmyu', 'っみゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmye', 'っみぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmyo', 'っみょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mma', 'っま', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmi', 'っみ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmu', 'っむ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mme', 'っめ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'mmo', 'っも', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yye', 'っいぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yya', 'っや', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yyu', 'っゆ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'yyo', 'っよ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rrya', 'っりゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rryi', 'っりぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rryu', 'っりゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rrye', 'っりぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rryo', 'っりょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rra', 'っら', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rri', 'っり', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rru', 'っる', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rre', 'っれ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'rro', 'っろ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwu', 'っう', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwyi', 'っゐ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwye', 'っゑ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwa', 'っわ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwi', 'っうぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwe', 'っうぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwo', 'っを', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwha', 'っうぁ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwhi', 'っうぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwhu', 'っう', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwhe', 'っうぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'wwho', 'っうぉ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccha', 'っちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cchi', 'っち', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cchu', 'っちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cche', 'っちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccho', 'っちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccya', 'っちゃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccyi', 'っちぃ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccyu', 'っちゅ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccye', 'っちぇ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccyo', 'っちょ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cca', 'っか', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cci', 'っし', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'ccu', 'っく', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cce', 'っせ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'cco', 'っこ', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'a', 'a', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'b', 'b', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'c', 'c', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'd', 'd', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'e', 'e', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'f', 'f', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'g', 'g', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'h', 'h', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'i', 'i', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'j', 'j', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'k', 'k', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'l', 'l', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'm', 'm', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'n', 'n', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'o', 'o', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'p', 'p', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'q', 'q', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'r', 'r', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 's', 's', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 't', 't', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'u', 'u', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'v', 'v', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'w', 'w', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'x', 'x', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'y', 'y', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'z', 'z', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'A', 'A', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'B', 'B', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'C', 'C', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'D', 'D', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'E', 'E', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'F', 'F', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'G', 'G', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'H', 'H', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'I', 'I', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'J', 'J', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'K', 'K', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'L', 'L', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'M', 'M', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'N', 'N', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'O', 'O', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'P', 'P', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'Q', 'Q', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'R', 'R', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'S', 'S', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'T', 'T', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'U', 'U', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'V', 'V', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'W', 'W', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'X', 'X', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'Y', 'Y', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, 'Z', 'Z', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '0', '0', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '1', '1', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '2', '2', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '3', '3', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '4', '4', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '5', '5', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '6', '6', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '7', '7', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '8', '8', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '9', '9', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '`', '`', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '~', '~', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '!', '!', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '@', '@', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '#', '#', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '$', '$', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '%', '%', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '^', '^', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '&', '&', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '*', '*', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '(', '(', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ')', ')', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '-', '-', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '_', '_', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '=', '=', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '+', '+', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '[', '[', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ']', ']', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '{', '{', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '}', '}', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '\\', '\\', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '|', '|', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ';', ';', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ':', ':', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '\'', '\'', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '\"', '\"', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ',', ',', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '<', '<', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '.', '.', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '>', '>', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '/', '/', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, '?', '?', 0),
		A3($GoldentTuft$elm_japanese_typing$Typing$Rule, ' ', ' ', 0)
	]);
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$round = _Basics_round;
var $GoldentTuft$elm_japanese_typing$Typing$setEfficiency = function (rules) {
	var outputLength = function (r) {
		return A3(
			$elm$core$Basics$clamp,
			1,
			9,
			$elm$core$String$length(r.output));
	};
	var calcEfficiencyA = function (r) {
		return $elm$core$String$length(r.output) / $elm$core$String$length(r.input);
	};
	var efficiencies = A2(
		$elm$core$List$map,
		function (rule) {
			return calcEfficiencyA(rule);
		},
		rules);
	var max = A2(
		$elm$core$Maybe$withDefault,
		1,
		$elm$core$List$maximum(efficiencies));
	var min = A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$minimum(efficiencies));
	var calcEfficiencyB = function (x) {
		return $elm$core$Basics$round(((x - min) / (max - min)) * 90);
	};
	return A3(
		$elm$core$List$map2,
		F2(
			function (rule, efficiency) {
				return _Utils_update(
					rule,
					{
						priority: calcEfficiencyB(efficiency) + outputLength(rule)
					});
			}),
		rules,
		efficiencies);
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $GoldentTuft$elm_japanese_typing$Typing$setFavorite = F3(
	function (fun, keys, rules) {
		return A2(
			$elm$core$List$map,
			function (rule) {
				return _Utils_update(
					rule,
					{
						priority: A2(
							$elm$core$List$any,
							function (k) {
								return A2(fun, k, rule.input);
							},
							keys) ? 1 : 0
					});
			},
			rules);
	});
var $GoldentTuft$elm_japanese_typing$Typing$setFavoriteKeys = F2(
	function (keys, rules) {
		return A3($GoldentTuft$elm_japanese_typing$Typing$setFavorite, $elm$core$String$contains, keys, rules);
	});
var $GoldentTuft$elm_japanese_typing$Typing$setFavoriteStart = F2(
	function (keys, rules) {
		return A3($GoldentTuft$elm_japanese_typing$Typing$setFavorite, $elm$core$String$startsWith, keys, rules);
	});
var $GoldentTuft$elm_japanese_typing$Typing$defaultPriorities = function (rules) {
	return A2(
		$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
		A2(
			$GoldentTuft$elm_japanese_typing$Typing$setFavoriteStart,
			_List_fromArray(
				['l']),
			$GoldentTuft$elm_japanese_typing$Typing$romanTable),
		A2(
			$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
			A2(
				$GoldentTuft$elm_japanese_typing$Typing$setFavoriteKeys,
				_List_fromArray(
					['cy']),
				$GoldentTuft$elm_japanese_typing$Typing$romanTable),
			A2(
				$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
				A2(
					$GoldentTuft$elm_japanese_typing$Typing$setFavoriteKeys,
					_List_fromArray(
						['sy', 'j', 'k', 'f', 'ty', 'si', 'se']),
					$GoldentTuft$elm_japanese_typing$Typing$romanTable),
				A2(
					$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
					$GoldentTuft$elm_japanese_typing$Typing$setEfficiency($GoldentTuft$elm_japanese_typing$Typing$romanTable),
					A2(
						$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
						_List_fromArray(
							[
								A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'n', 'ん', 3),
								A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'nn', 'ん', 2),
								A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'xn', 'ん', 1)
							]),
						rules)))));
};
var $author$project$CountDown$Timer = function (a) {
	return {$: 'Timer', a: a};
};
var $author$project$CountDown$init = F3(
	function (start, initialTime, interval) {
		return $author$project$CountDown$Timer(
			{initialTime: initialTime, interval: interval, start: start, time: initialTime});
	});
var $GoldentTuft$elm_japanese_typing$Typing$ConvertBuf = F3(
	function (inputBuffer, tmpFixed, candidates) {
		return {candidates: candidates, inputBuffer: inputBuffer, tmpFixed: tmpFixed};
	});
var $GoldentTuft$elm_japanese_typing$Typing$Data = function (a) {
	return {$: 'Data', a: a};
};
var $GoldentTuft$elm_japanese_typing$Typing$Waiting = {$: 'Waiting'};
var $elm$core$List$sortWith = _List_sortWith;
var $GoldentTuft$elm_japanese_typing$Typing$newData = F2(
	function (words, rules) {
		var sf = F2(
			function (a, b) {
				return A2($elm$core$Basics$compare, b.priority, a.priority);
			});
		return $GoldentTuft$elm_japanese_typing$Typing$Data(
			{
				convertBuf: A3(
					$GoldentTuft$elm_japanese_typing$Typing$ConvertBuf,
					'',
					$elm$core$Maybe$Nothing,
					A2($elm$core$List$sortWith, sf, rules)),
				fixedWords: '',
				history: '',
				restWords: words,
				rules: A2($elm$core$List$sortWith, sf, rules),
				state: $GoldentTuft$elm_japanese_typing$Typing$Waiting
			});
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $GoldentTuft$elm_japanese_typing$Typing$setPriorities = F2(
	function (printRules, rules) {
		return A2(
			$elm$core$List$map,
			function (rule) {
				return _Utils_update(
					rule,
					{
						priority: A2($GoldentTuft$elm_japanese_typing$Typing$getPriority, rule, printRules)
					});
			},
			rules);
	});
var $author$project$Page$TypeShortWord$initCustomTypingWords = function (words) {
	var printRules = A2(
		$GoldentTuft$elm_japanese_typing$Typing$insertLowPriorities,
		$GoldentTuft$elm_japanese_typing$Typing$defaultPriorities($GoldentTuft$elm_japanese_typing$Typing$romanTable),
		A2(
			$GoldentTuft$elm_japanese_typing$Typing$setPriorities,
			_List_fromArray(
				[
					A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'n', 'ん', 3),
					A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'xn', 'ん', 2),
					A3($GoldentTuft$elm_japanese_typing$Typing$PrintRule, 'nn', 'ん', 1)
				]),
			$GoldentTuft$elm_japanese_typing$Typing$romanTable));
	var isw = A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, words);
	var fun = function (_v0) {
		var i = _v0.a;
		var d = _v0.b;
		return {
			finishTime: $elm$time$Time$millisToPosix(0),
			index: i,
			miss: 0,
			rhythmTimer: (!i) ? A3($author$project$CountDown$init, true, 1, 1) : A3($author$project$CountDown$init, true, 250, 50),
			startTime: $elm$time$Time$millisToPosix(0),
			typingData: A2($GoldentTuft$elm_japanese_typing$Typing$newData, d.wordForInput, printRules),
			wordForView: d.wordForView
		};
	};
	return {
		finish: _List_Nil,
		miss: 0,
		missed: false,
		rest: A2($elm$core$List$map, fun, isw)
	};
};
var $author$project$Page$TypeShortWord$initialCountDownTimer = A3($author$project$CountDown$init, false, 3000, 1000);
var $author$project$Data$ShortWord$Word = F2(
	function (wordForView, wordForInput) {
		return {wordForInput: wordForInput, wordForView: wordForView};
	});
var $author$project$Emb$ShortWord1$list = _List_fromArray(
	[
		_Utils_Tuple2('図ζ書ζ館で待ったをかけるビジネス', 'としょかんでまったをかけるびじねす'),
		_Utils_Tuple2('鳴りζ物入りのスクーターが手にζ余る', 'なりものいりのすくーたーがてにあまる'),
		_Utils_Tuple2('ゴリ押しする希ζ望で死ζ角にζ入る', 'ごりおしするきぼうでしかくにはいる'),
		_Utils_Tuple2('びしょぬれのぞうきんをζ八ζ方ζ塞がり', 'びしょぬれのぞうきんをはっぽうふさがり'),
		_Utils_Tuple2('ζ策をζ弄するζ鼻ζ息がζ荒いζζ羊', 'さくをろうするはないきがあらいひつじ'),
		_Utils_Tuple2('油ζ断もζ隙もないてこ入れをする', 'ゆだんもすきもないてこいれをする'),
		_Utils_Tuple2('ζ進ζ退きわまるζ穀つぶしのζ孫', 'しんたいきわまるごくつぶしのまご'),
		_Utils_Tuple2('ありったけζ製ζ造されるあぶくζ銭', 'ありったけせいぞうされるあぶくぜに'),
		_Utils_Tuple2('あらかじめζ余りζ物にはζ福がある', 'あらかじめあまりものにはふくがある'),
		_Utils_Tuple2('一ζζζ丁目のζ声がつぶれるζ神ζ様', 'いっちょうめのこえがつぶれるかみさま'),
		_Utils_Tuple2('これだけζ雁ζ首をζ揃えるワープロ', 'これだけがんくびをそろえるわーぷろ'),
		_Utils_Tuple2('名乗りを上げるζ同じζ穴のζζ狢', 'なのりをあげるおなじあなのむじな'),
		_Utils_Tuple2('ひたすらζ隅に置けない生けるζζζ屍', 'ひたすらすみにおけないいけるしかばね'),
		_Utils_Tuple2('ζ折ζ紙付きのトイレットペーパー', 'おりがみつきのといれっとぺーぱー'),
		_Utils_Tuple2('ζζ小異を捨ててζ大ζ同につくζ我ζ々', 'しょういをすててだいどうにつくわれわれ'),
		_Utils_Tuple2('ちょろちょろ時ζ間をζ稼ぐζ操作', 'ちょろちょろじかんをかせぐそうさ'),
		_Utils_Tuple2('ようやくζ金はζ天下のζ回りζ物', 'ようやくかねはてんかのまわりもの'),
		_Utils_Tuple2('しばしばζζ心をζζ傾けるζ善ζ悪', 'しばしばこころをかたむけるぜんあく'),
		_Utils_Tuple2('ζζ心はζ往ζζ生ζ際がζ悪い時ζ刻', 'こころはおうじょうぎわがわるいじこく'),
		_Utils_Tuple2('どっちもどっちを鵜呑みにするζ末っ子', 'どっちもどっちをうのみにするすえっこ'),
		_Utils_Tuple2('ζ運をζ天にζ任せるζ弁が立つじいさん', 'うんをてんにまかせるべんがたつじいさん'),
		_Utils_Tuple2('あの血もζζ涙もない持ちζ味を出す', 'あのちもなみだもないもちあじをだす'),
		_Utils_Tuple2('たまにζ人目につくζ背ζ水のζ陣', 'たまにひとめにつくはいすいのじん'),
		_Utils_Tuple2('手のζ平をζ返すびっくりする押しの一ζ手', 'てのひらをかえすびっくりするおしのいって'),
		_Utils_Tuple2('泣く子もζ黙るζ夢をζ描く日がζ浅いかたぎ', 'なくこもだまるゆめをえがくひがあさいかたぎ'),
		_Utils_Tuple2('あっと言わせるζζ週ζ刊ζ甘いζ汁を吸う', 'あっといわせるしゅうかんあまいしるをすう'),
		_Utils_Tuple2('ζ苦ζ虫を噛みつぶすζ夏のζζ商ζ店ζ街', 'にがむしをかみつぶすなつのしょうてんがい'),
		_Utils_Tuple2('名を借りるアイデアでζζ涎を垂らす', 'なをかりるあいであでよだれをたらす'),
		_Utils_Tuple2('ζ一二をζζ争うζζ頭がζ固い組ζ織', 'いちにをあらそうあたまがかたいそしき'),
		_Utils_Tuple2('気にかけるボーナスはζ嘘でζ固める', 'きにかけるぼーなすはうそでかためる'),
		_Utils_Tuple2('さもないとζ筋書きζ通りζ青ζ天のζ霹ζ靂', 'さもないとすじがきどおりせいてんのへきれき'),
		_Utils_Tuple2('ζζ港でえびでζ鯛を釣る自ζ由をζ口にする', 'みなとでえびでたいをつるじゆうをくちにする'),
		_Utils_Tuple2('ζ胸に秘めるζ健ζ康のζ種を明かす', 'むねにひめるけんこうのたねをあかす'),
		_Utils_Tuple2('ζ謎をかける図ζ形にζ足留めを食う', 'なぞをかけるずけいにあしどめをくう'),
		_Utils_Tuple2('ζζ頭をζ抱えるスポンサーが無きにしもζ非ず', 'あたまをかかえるすぽんさーがなきにしもあらず'),
		_Utils_Tuple2('ζ顔を見せる知るζ人ぞ知るさすらうζ箱', 'かおをみせるしるひとぞしるさすらうはこ'),
		_Utils_Tuple2('ζζ命をζ削るドライヤーが気をζ静める', 'いのちをけずるどらいやーがきをしずめる'),
		_Utils_Tuple2('ζ砂を噛むようなありふれた不ζ幸ζζ中のζζ幸い', 'すなをかむようなありふれたふこうちゅうのさいわい'),
		_Utils_Tuple2('うらやましいζζ急ζ所を衝くやりくり', 'うらやましいきゅうしょをつくやりくり'),
		_Utils_Tuple2('ばんそうこうの名に恥じないζ顔がζ揃う', 'ばんそうこうのなにはじないかおがそろう'),
		_Utils_Tuple2('血のにじむようなプロレスでとばっちりを食う', 'ちのにじむようなぷろれすでとばっちりをくう'),
		_Utils_Tuple2('マスコミはζ陰ζ口をζ叩くζ本ζ腰を入れる', 'ますこみはかげぐちをたたくほんごしをいれる'),
		_Utils_Tuple2('気がζ回るリクエストをζ勘ζζ定に入れる', 'きがまわるりくえすとをかんじょうにいれる'),
		_Utils_Tuple2('ζ毎ζ日うぬぼれにζ耳を澄ます烏ζ合のζζ衆', 'まいにちうぬぼれにみみをすますうごうのしゅう'),
		_Utils_Tuple2('ささやくζ玉のζ輿にζζ心をζ鬼にする', 'ささやくたまのこしにこころをおににする'),
		_Utils_Tuple2('ζ便利にζ明日はζ明日のζ風が吹くとζ申す', 'べんりにあしたはあしたのかぜがふくともうす'),
		_Utils_Tuple2('ましてζ煙に巻くこちらζ鼻であしらう', 'ましてけむにまくこちらはなであしらう'),
		_Utils_Tuple2('ζ痒いζζ所に手がζ届くζ液ζ体ζ憲ζ法', 'かゆいところにてがとどくえきたいけんぽう'),
		_Utils_Tuple2('ζ人目をζ盗むドライブで布ζ石を打つ', 'ひとめをぬすむどらいぶでふせきをうつ'),
		_Utils_Tuple2('もちろんζ三ζ人寄ればζ文ζ殊の知恵', 'もちろんさんにんよればもんじゅのちえ'),
		_Utils_Tuple2('それとも目に浮かぶζ藁にもすがるシリーズ', 'それともめにうかぶわらにもすがるしりーず'),
		_Utils_Tuple2('血はζζ争えないζ念にはζ念を入れる着るテント', 'ちはあらそえないねんにはねんをいれるきるてんと'),
		_Utils_Tuple2('一ζζ石をζ投じるζ元も子もなくなるζ班ζζ長', 'いっせきをとうじるもともこもなくなるはんちょう'),
		_Utils_Tuple2('ζ舌が肥えるモデル、ハンバーグζ万事ζζ休す', 'したがこえるもでる、はんばーぐばんじきゅうす'),
		_Utils_Tuple2('気がζ弱いζ口を割るあさりはζ缶ζ詰になる', 'きがよわいくちをわるあさりはかんづめになる'),
		_Utils_Tuple2('しくじるζζζ弟に目くじらを立てるバター', 'しくじるおとうとにめくじらをたてるばたー'),
		_Utils_Tuple2('ホッチキスがζ人手にζ渡るとζ一ζ皮剥ける', 'ほっちきすがひとでにわたるとひとかわむける'),
		_Utils_Tuple2('そのくせζζ暖かいζ朝にはヘドが出る', 'そのくせあたたかいあさにはへどがでる'),
		_Utils_Tuple2('ζζ秒読みにζ入る木を見てζ森を見ず', 'びょうよみにはいるきをみてもりをみず'),
		_Utils_Tuple2('ついでに手に乗るζ粉ζ雪のなれの果て', 'ついでにてにのるこなゆきのなれのはて'),
		_Utils_Tuple2('ζ磨きがかかるζζ狐につままれるζ副作ζ用', 'みがきがかかるきつねにつままれるふくさよう'),
		_Utils_Tuple2('ζ琴ζ線に触れるつぶらなζ音ζ楽が手をζ離れる', 'きんせんにふれるつぶらなおんがくがてをはなれる'),
		_Utils_Tuple2('ちなみにζζ所変わればζ品変わるコンビニ', 'ちなみにところかわればしなかわるこんびに'),
		_Utils_Tuple2('ζ引ζ導をζ渡すζζ心を打つ差しζ金ζ方ζ法', 'いんどうをわたすこころをうつさしがねほうほう'),
		_Utils_Tuple2('ζζ命を懸けるとげとげしい無いζ物ねだり', 'いのちをかけるとげとげしいないものねだり'),
		_Utils_Tuple2('いわば気がζ進まないもじもじζ乾ζ坤一ζζ擲', 'いわばきがすすまないもじもじけんこんいってき'),
		_Utils_Tuple2('火を見るよりζ明らかランキングをチェック', 'ひをみるよりあきらからんきんぐをちぇっく'),
		_Utils_Tuple2('ζζ枕をζ並べるζ危ないベーコンに目を凝らす', 'まくらをならべるあぶないべーこんにめをこらす'),
		_Utils_Tuple2('蜘蛛の子を散らすζζ嵐のζ前のζ静けさ', 'くものこをちらすあらしのまえのしずけさ'),
		_Utils_Tuple2('悲ζ鳴を上げる機にζζ乗ずるナメクジ', 'ひめいをあげるきにじょうずるなめくじ'),
		_Utils_Tuple2('いい子になるζ幼児を見て見ぬふりをする', 'いいこになるようじをみてみぬふりをする'),
		_Utils_Tuple2('おおよそ二ζ足のわらじを履くパスポート', 'おおよそにそくのわらじをはくぱすぽーと'),
		_Utils_Tuple2('一ζζ線をζ画すζζ調子がいいつんつん。', 'いっせんをかくすちょうしがいいつんつん。'),
		_Utils_Tuple2('ζ臨機ζ応ζ変に羽目をζ外すζ冷たいζ心ζ臓', 'りんきおうへんにはめをはずすつめたいしんぞう'),
		_Utils_Tuple2('手が込んだおζ安い御ζ用でζζ正ζ面を切る', 'てがこんだおやすいごようでしょうめんをきる'),
		_Utils_Tuple2('あこがれの、ほぼ好きこそζ物のζζ上手なれ', 'あこがれの、ほぼすきこそもののじょうずなれ'),
		_Utils_Tuple2('突ζζζ拍子もないζ初ζ心にζ返るζζ漁師', 'とっぴょうしもないしょしんにかえるりょうし'),
		_Utils_Tuple2('ζ射ζ程ζ距離にζ入るζζ命のζ洗ζ濯', 'しゃていきょりにはいるいのちのせんたく'),
		_Utils_Tuple2('ζ張合いがないζ郷に入ってはζ郷にζζ従え', 'はりあいがないごうにいってはごうにしたがえ'),
		_Utils_Tuple2('ζ箔がつく気のせいに身を入れるζ内ζ閣', 'はくがつくきのせいにみをいれるないかく'),
		_Utils_Tuple2('おζ言葉にζ甘えてζ漁夫の利まっしぐら', 'おことばにあまえてぎょふのりまっしぐら'),
		_Utils_Tuple2('もはやζζ宙に浮くζ色眼ζ鏡で見るチャンス', 'もはやちゅうにうくいろめがねでみるちゃんす'),
		_Utils_Tuple2('ζ筆ζ舌に尽くしがたいζ猫なでζ声がζ誕ζζ生', 'ひつぜつにつくしがたいねこなでごえがたんじょう'),
		_Utils_Tuple2('言わずもがなζ波に乗るζ何をζ隠そうζ姉', 'いわずもがななみにのるなにをかくそうあね'),
		_Utils_Tuple2('ζ筋がいいζ金ζ魚のフンはζ垂ζ涎のζ的', 'すじがいいきんぎょのふんはすいぜんのまと'),
		_Utils_Tuple2('ζζζ腸がちぎれる目がくらむζζ着ζ陸', 'はらわたがちぎれるめがくらむちゃくりく'),
		_Utils_Tuple2('こんにちは飼いζ犬に手を噛まれる衣ζζ装', 'こんにちはかいいぬにてをかまれるいしょう'),
		_Utils_Tuple2('いきなりζ闇にζζ葬るおζ茶の子さいさい', 'いきなりやみにほうむるおちゃのこさいさい'),
		_Utils_Tuple2('ζ旗揚げするζ高嶺のζ花はζ牙を研ぐ', 'はたあげするたかねのはなはきばをとぐ'),
		_Utils_Tuple2('出るζζ所へ出るζ極ζ楽とんぼをζ耳にする', 'でるところへでるごくらくとんぼをみみにする'),
		_Utils_Tuple2('時ζζ流に乗る踏ん切りがつかないζ主ζζ張', 'じりゅうにのるふんぎりがつかないしゅちょう'),
		_Utils_Tuple2('ζ何はともあれζ風ζ通しがいいζ網を張る', 'なにはともあれかぜとおしがいいあみをはる'),
		_Utils_Tuple2('見るζ影もない一ζζ石二ζζ鳥でくつろぐ', 'みるかげもないいっせきにちょうでくつろぐ'),
		_Utils_Tuple2('ζ桁がζ違うζ大きなζ顔をする地ζζ球', 'けたがちがうおおきなかおをするちきゅう'),
		_Utils_Tuple2('寝ζ耳にζ水がζ耳にζ残る記ζ念にζ休み', 'ねみみにみずがみみにのこるきねんにやすみ'),
		_Utils_Tuple2('得ζ体が知れないネジがζ緩むζ後押しをする', 'えたいがしれないねじがゆるむあとおしをする'),
		_Utils_Tuple2('いやおうなしにζζ命を投げ出すζ息が合う', 'いやおうなしにいのちをなげだすいきがあう'),
		_Utils_Tuple2('スーパーζ光ζ陰矢のζ如しにζ花を咲かせる', 'すーぱーこういんやのごとしにはなをさかせる'),
		_Utils_Tuple2('ζ情けζ容ζ赦もないζ大はζζ小を兼ねるζ筋ζ肉', 'なさけようしゃもないだいはしょうをかねるきんにく'),
		_Utils_Tuple2('しのぎをζ削るクリックにζ重きを置くζ陛下', 'しのぎをけずるくりっくにおもきをおくへいか'),
		_Utils_Tuple2('もっとζ悟りをζ開くサービスでζ株を上げる', 'もっとさとりをひらくさーびすでかぶをあげる'),
		_Utils_Tuple2('ζ暗ζζ礁に乗り上げるζ花よりζ団子のたぐい', 'あんしょうにのりあげるはなよりだんごのたぐい'),
		_Utils_Tuple2('ζ的を射るζ道ζ徳で馬鹿を見るフクロウ', 'まとをいるどうとくでばかをみるふくろう'),
		_Utils_Tuple2('それぞれものすごいζ糠にζ釘がζ頼みのζ綱', 'それぞれものすごいぬかにくぎがたのみのつな'),
		_Utils_Tuple2('ζ弾みがつくとんちんかんに不ζ覚を取る', 'はずみがつくとんちんかんにふかくをとる'),
		_Utils_Tuple2('スリッパではしごするとζ大目ζ玉を食う', 'すりっぱではしごするとおおめだまをくう'),
		_Utils_Tuple2('ζζ命ζ拾いをするおいしいボタンの掛けζ違い', 'いのちびろいをするおいしいぼたんのかけちがい'),
		_Utils_Tuple2('インスタントでζ爪のζ垢をζ煎じて飲む', 'いんすたんとでつめのあかをせんじてのむ'),
		_Utils_Tuple2('言うなればζ芸がζ細かいさりげないジャガイモ', 'いうなればげいがこまかいさりげないじゃがいも'),
		_Utils_Tuple2('気が気でないζ情けはζ人のζ為ならずビデオ', 'きがきでないなさけはひとのためならずびでお'),
		_Utils_Tuple2('ζ例に漏れずζ割に合わないζζ茨のζ道', 'れいにもれずわりにあわないいばらのみち'),
		_Utils_Tuple2('さようならζζ心にζ残るどんぶりζ勘ζζ定', 'さようならこころにのこるどんぶりかんじょう'),
		_Utils_Tuple2('おのずとζ白い歯を見せるζ御の字タイム', 'おのずとしろいはをみせるおんのじたいむ'),
		_Utils_Tuple2('一ζ糸ζ乱れずζ色気を出すヒップスタート', 'いっしみだれずいろけをだすひっぷすたーと'),
		_Utils_Tuple2('ζ肌でζ感じるζ三ζ角にζ肝を冷やす', 'はだでかんじるさんかくにきもをひやす'),
		_Utils_Tuple2('気ζ勢を上げるハゲがζζ消ζ息を絶つ', 'きせいをあげるはげがしょうそくをたつ'),
		_Utils_Tuple2('ζζ性懲りもなくネズミに知恵を借りる', 'しょうこりもなくねずみにちえをかりる'),
		_Utils_Tuple2('ζζ頭が真っζ白になるζ口から出ζ任せ', 'あたまがまっしろになるくちからでまかせ'),
		_Utils_Tuple2('湯ζ水のようにζ趣ζ向を凝らすパイロット', 'ゆみずのようにしゅこうをこらすぱいろっと'),
		_Utils_Tuple2('いよいよζ我をζ忘れる八ζζ方美ζ人。', 'いよいよわれをわすれるはっぽうびじん。'),
		_Utils_Tuple2('メリハリをつける無ζ礼がζ満ζ更でもない', 'めりはりをつけるぶれいがまんざらでもない'),
		_Utils_Tuple2('ζ後ζ味がζ悪いζ完ζ成にζ口をζζ慎む', 'あとあじがわるいかんせいにくちをつつしむ'),
		_Utils_Tuple2('ζ悪ζ銭身につかずをζ覚えるちゃらちゃら', 'あくせんみにつかずをおぼえるちゃらちゃら'),
		_Utils_Tuple2('ζ何はさておきカレーで露ζ命をつなぐ', 'なにはさておきかれーでろめいをつなぐ'),
		_Utils_Tuple2('ζ塩おむすびにζζ終止符を打つζ牧師', 'しおおむすびにしゅうしふをうつぼくし'),
		_Utils_Tuple2('ζ一ζ泡吹かせるζζ忠ζ誠にζ根を詰める', 'ひとあわふかせるちゅうせいにこんをつめる'),
		_Utils_Tuple2('視野がζ広いζ君ζ臨でふざけるζ演ζ劇', 'しやがひろいくんりんでふざけるえんげき'),
		_Utils_Tuple2('打って変わってインテリアはζ金のなる木', 'うってかわっていんてりあはかねのなるき'),
		_Utils_Tuple2('しっぺζ返しをするζ仏ζ像にζζ兜を脱ぐ', 'しっぺがえしをするぶつぞうにかぶとをぬぐ'),
		_Utils_Tuple2('生きたζ心地もしないやけを起こす季ζ節', 'いきたここちもしないやけをおこすきせつ'),
		_Utils_Tuple2('だるいζ笑うζ門にはζ福来たるの派ζ生', 'だるいわらうかどにはふくきたるのはせい'),
		_Utils_Tuple2('明けても暮れてもζζ心を込める見切り発ζζ車', 'あけてもくれてもこころをこめるみきりはっしゃ'),
		_Utils_Tuple2('ζ門をζ叩くζ苦しいときのζ神ζ頼み', 'もんをたたくくるしいときのかみだのみ'),
		_Utils_Tuple2('地の利を得る机ζζ上のζ空ζ論をζ提ζζ供', 'ちのりをえるきじょうのくうろんをていきょう'),
		_Utils_Tuple2('ζζ烏のζζ行ζ水のζ足ζ元にもζ及ばない', 'からすのぎょうずいのあしもとにもおよばない'),
		_Utils_Tuple2('ζ風の吹きζ回しのζ風向きがζ悪いζ谷', 'かぜのふきまわしのかざむきがわるいたに'),
		_Utils_Tuple2('きらめくにぎやかζ喧嘩ζζ両ζ成ζ敗', 'きらめくにぎやかけんかりょうせいばい'),
		_Utils_Tuple2('ζζ良ζ薬ζ口にζ苦しで行き当たりばったり', 'りょうやくくちににがしでいきあたりばったり'),
		_Utils_Tuple2('気の利いた追い打ちをかけるζ腕が上がる', 'きのきいたおいうちをかけるうでがあがる'),
		_Utils_Tuple2('さいころでζ骨のζ髄までζ本音を吐くζ労ζ働', 'さいころでほねのずいまでほんねをはくろうどう'),
		_Utils_Tuple2('化けのζ皮がはがれるζ鬼の目にもζζ涙', 'ばけのかわがはがれるおにのめにもなみだ'),
		_Utils_Tuple2('遺ζ憾なく地ζ獄ζ耳をζ顎でζ使う', 'いかんなくじごくみみをあごでつかう'),
		_Utils_Tuple2('急いてはζ事を仕ζ損じるとζ道ζ草を食う', 'せいてはことをしそんじるとみちくさをくう'),
		_Utils_Tuple2('ひねるユーモアにζ太鼓ζ判を押す土ζζ俵', 'ひねるゆーもあにたいこばんをおすどひょう'),
		_Utils_Tuple2('ζ辺りζ構わず向かうところζ敵なしのζ空', 'あたりかまわずむかうところてきなしのそら'),
		_Utils_Tuple2('おもむろに焼きを入れる八百ζζ長でζ人目を引く', 'おもむろにやきをいれるやおちょうでひとめをひく'),
		_Utils_Tuple2('ζ明るいζ右からζζ左で名をあげる', 'あかるいみぎからひだりでなをあげる'),
		_Utils_Tuple2('ζ顔ζ色をζζ窺うζ袖のζ下がζ板につく', 'かおいろをうかがうそでのしたがいたにつく'),
		_Utils_Tuple2('手のζζ施しようがないζ垂ζζ直に目をζ背ける', 'てのほどこしようがないすいちょくにめをそむける'),
		_Utils_Tuple2('子ζ供だましで暮らしを立てるさくさくリズム', 'こどもだましでくらしをたてるさくさくりずむ'),
		_Utils_Tuple2('ζ片意地を張るミュージックトラブル', 'かたいじをはるみゅーじっくとらぶる'),
		_Utils_Tuple2('いのζ一ζ番にζ道をζ開くマネージャー', 'いのいちばんにみちをひらくまねーじゃー'),
		_Utils_Tuple2('聞きしにζ勝るティッシュを折るζ大工', 'ききしにまさるてぃっしゅをおるだいく'),
		_Utils_Tuple2('ζ先が見えるとアラをζ探す詩ζ人が言う', 'さきがみえるとあらをさがすしじんがいう'),
		_Utils_Tuple2('いっそゴボウとζ金の切れ目がζ縁の切れ目', 'いっそごぼうとかねのきれめがえんのきれめ'),
		_Utils_Tuple2('ζ君子ζ危うきにζ近寄らずそのままζ登る', 'くんしあやうきにちかよらずそのままのぼる'),
		_Utils_Tuple2('このほどζ国ζ破れてζ山河ありのζ感ζ覚', 'このほどくにやぶれてさんがありのかんかく'),
		_Utils_Tuple2('苦ζ言をζ呈するマウスのζζ話がわかる', 'くげんをていするまうすのはなしがわかる'),
		_Utils_Tuple2('このごろはζ投ζζ票でζ白羽の矢が立つ', 'このごろはとうひょうでしらはのやがたつ'),
		_Utils_Tuple2('引き合いに出すと血が沸くζζ教師がζ帰る', 'ひきあいにだすとちがわくきょうしがかえる'),
		_Utils_Tuple2('アレルギーでζ後に引けないζ采ζ配を振る', 'あれるぎーであとにひけないさいはいをふる'),
		_Utils_Tuple2('痩せのζ大食いがζ水のζ泡になるビール', 'やせのおおぐいがみずのあわになるびーる'),
		_Utils_Tuple2('ζ脳裏に焼き付くζ道路をあいにくボツにする', 'のうりにやきつくどうろをあいにくぼつにする'),
		_Utils_Tuple2('ツキがζ回るクッキーはζ口添えをする', 'つきがまわるくっきーはくちぞえをする'),
		_Utils_Tuple2('のけぞるうじうじに異をζ唱える日ζ課', 'のけぞるうじうじにいをとなえるにっか'),
		_Utils_Tuple2('理ζ解にζ苦しむζ暑さζ寒さも彼ζ岸まで', 'りかいにくるしむあつささむさもひがんまで'),
		_Utils_Tuple2('いちごζ農ζζ業はζ役ζ者がζ一ζ枚ζ上', 'いちごのうぎょうはやくしゃがいちまいうえ'),
		_Utils_Tuple2('ζ最ζ初でζ最後の犠ζ牲をζ払うさんご', 'さいしょでさいごのぎせいをはらうさんご'),
		_Utils_Tuple2('ζ論よりζζ証拠に一ζ矢をζ報いるちくちく', 'ろんよりしょうこにいっしをむくいるちくちく'),
		_Utils_Tuple2('ζ夜にζ先んずればζ人をζ制すζ出ζζ席', 'よるにさきんずればひとをせいすしゅっせき'),
		_Utils_Tuple2('おじいさんはζ問ζ答無ζ用でζ相ζ槌を打つ', 'おじいさんはもんどうむようであいづちをうつ'),
		_Utils_Tuple2('一ζζ世をζ風靡するパーマζ大ζ統ζζ領', 'いっせいをふうびするぱーまだいとうりょう'),
		_Utils_Tuple2('ζ幸ζ福にメスを入れるζ人ζ間のζ歌', 'こうふくにめすをいれるにんげんのうた'),
		_Utils_Tuple2('ζ猫もζζ杓子も歯ζζ車が噛み合う四ζ日', 'ねこもしゃくしもはぐるまがかみあうよっか'),
		_Utils_Tuple2('ζ一ζ翼をζ担う小ζ鳥はζ風ζ穴を開ける', 'いちよくをになうことりはかざあなをあける'),
		_Utils_Tuple2('馬耳ζ東ζ風でもζ神ζ経がζ細かいライバル', 'ばじとうふうでもしんけいがこまかいらいばる'),
		_Utils_Tuple2('一ζ糸まとわずζ足踏みするζ外ζζ出', 'いっしまとわずあしぶみするがいしゅつ'),
		_Utils_Tuple2('まんじゅうにζ色をつける手を打つζζ兄ζ弟', 'まんじゅうにいろをつけるてをうつきょうだい'),
		_Utils_Tuple2('ζ言葉が過ぎるζ質ζ問でもζζζ懐がζ深い', 'ことばがすぎるしつもんでもふところがふかい'),
		_Utils_Tuple2('ζ伏ζ線を張るめまいは目ζζ頭を押さえる', 'ふくせんをはるめまいはめがしらをおさえる'),
		_Utils_Tuple2('ζ鼻のζ下を伸ばすダイエットにζ熱を上げる', 'はなのしたをのばすだいえっとにねつをあげる'),
		_Utils_Tuple2('沽ζ券にかかわるζ面と向かってステップ', 'こけんにかかわるめんとむかってすてっぷ'),
		_Utils_Tuple2('みるみるエチケットをζ声をζ大にする', 'みるみるえちけっとをこえをだいにする'),
		_Utils_Tuple2('気がζ沈むラジオにζζ命をζ預ける', 'きがしずむらじおにいのちをあずける'),
		_Utils_Tuple2('ζ涼しいζ顔ζ推ζ進をζ顎でしゃくる', 'すずしいかおすいしんをあごでしゃくる'),
		_Utils_Tuple2('ζ声を枯らすさわやかのネタが割れるζζ幸せ', 'こえをからすさわやかのねたがわれるしあわせ'),
		_Utils_Tuple2('ζ申しζ分がない汚ζ点をζ残すζ印ζ刷', 'もうしぶんがないおてんをのこすいんさつ'),
		_Utils_Tuple2('ζζ頭でっかちでζ虻ζ蜂取らずのしぐさ', 'あたまでっかちであぶはちとらずのしぐさ'),
		_Utils_Tuple2('じかにζ一ζ肌脱ぐトレーナーのスタミナ', 'じかにひとはだぬぐとれーなーのすたみな'),
		_Utils_Tuple2('目を見張る火にζζ油をζ注ぐからくり', 'めをみはるひにあぶらをそそぐからくり'),
		_Utils_Tuple2('そのイビキもついにζ年貢のζ納めどき', 'そのいびきもついにねんぐのおさめどき'),
		_Utils_Tuple2('ケチをつけるくちばしに目をむくスポーツ', 'けちをつけるくちばしにめをむくすぽーつ'),
		_Utils_Tuple2('かまぼこもζζ鰯のζζ頭もζ信ζ心から', 'かまぼこもいわしのあたまもしんじんから'),
		_Utils_Tuple2('やむをえずζ筆を折るけいれんオヤジζζ失う', 'やむをえずふでをおるけいれんおやじうしなう'),
		_Utils_Tuple2('メロンで見栄を張るζ文化に歯止めをかける', 'めろんでみえをはるぶんかにはどめをかける'),
		_Utils_Tuple2('チャンピオンはζ初めて歩ζζ調を合わせる', 'ちゃんぴおんははじめてほちょうをあわせる'),
		_Utils_Tuple2('ζζ集ζ団で吐き気をζζ催す折り合いをつける', 'しゅうだんではきけをもよおすおりあいをつける'),
		_Utils_Tuple2('ζζ心がζ洗われるトンネルをもたらす', 'こころがあらわれるとんねるをもたらす'),
		_Utils_Tuple2('ζ速い地ζ団駄を踏むからかうζ友ζ達', 'はやいじだんだをふむからかうともだち'),
		_Utils_Tuple2('ζ腕ζζ力にζζ訴えるζ応ζ用にζ精が出る', 'わんりょくにうったえるおうようにせいがでる'),
		_Utils_Tuple2('ζ虚を衝くζ腰がζ低いまじめのζ童話', 'きょをつくこしがひくいまじめのどうわ'),
		_Utils_Tuple2('あくびを噛みζ殺すおしゃべりζ先ζ生', 'あくびをかみころすおしゃべりせんせい'),
		_Utils_Tuple2('途ζ方に暮れるζ親子が気を取りζ直す', 'とほうにくれるおやこがきをとりなおす'),
		_Utils_Tuple2('ζ樹ζ木でぎこちない泣くに泣けないセミ', 'じゅもくでぎこちないなくになけないせみ'),
		_Utils_Tuple2('ζ物のζ弾みでζ白ζ黒をハッキリさせる', 'もののはずみでしろくろをはっきりさせる'),
		_Utils_Tuple2('捨てるζ神あればζ拾うζ神ありおじさん', 'すてるかみあればひろうかみありおじさん'),
		_Utils_Tuple2('ふびんでζ何がζ何でもζ脚ζζ光を浴びる', 'ふびんでなにがなんでもきゃっこうをあびる'),
		_Utils_Tuple2('ζζ再びζ流れを汲むζ凄いζ筋をζ通す', 'ふたたびながれをくむすごいすじをとおす'),
		_Utils_Tuple2('ζ溺れるζ者はζ藁をもζ掴むζ腕を鳴らす', 'おぼれるものはわらをもつかむうでをならす'),
		_Utils_Tuple2('ζζ心を惹かれるζ何でもありスイッチ', 'こころをひかれるなんでもありすいっち'),
		_Utils_Tuple2('ごζ愛ζζ敬でわざとζ恨みを買う技ζζ術', 'ごあいきょうでわざとうらみをかうぎじゅつ'),
		_Utils_Tuple2('ζ大きいおできでζζ涙に暮れるバイキング', 'おおきいおできでなみだにくれるばいきんぐ'),
		_Utils_Tuple2('ζ採ζζ集も地ζ獄の沙汰もζ金次ζ第', 'さいしゅうもじごくのさたもかねしだい'),
		_Utils_Tuple2('かたつむりはζ穴があったらζ入りたい', 'かたつむりはあながあったらはいりたい'),
		_Utils_Tuple2('ζ腹のζ虫がζ治まらないζ暴れるひまわり', 'はらのむしがおさまらないあばれるひまわり'),
		_Utils_Tuple2('ハラハラとぼちぼちをζ天ζ秤にかける', 'はらはらとぼちぼちをてんびんにかける'),
		_Utils_Tuple2('絵に描いたζ餅でζ育てるすっぽかすヤクザ', 'えにかいたもちでそだてるすっぽかすやくざ'),
		_Utils_Tuple2('ζ大見得を切るレッスンにζζ力を入れる', 'おおみえをきるれっすんにちからをいれる'),
		_Utils_Tuple2('ζ痛いζζ所を衝く委ζ員のζ怒りを買う', 'いたいところをつくいいんのいかりをかう'),
		_Utils_Tuple2('工ζ夫を凝らすロボットにζ信を問うζ存ζ在', 'くふうをこらすろぼっとにしんをとうそんざい'),
		_Utils_Tuple2('抜け目がないζζ両手にζ花のきらきらζ生ζ活', 'ぬけめがないりょうてにはなのきらきらせいかつ'),
		_Utils_Tuple2('借りてきたζ猫がしぶといζ梨のつぶて', 'かりてきたねこがしぶといなしのつぶて'),
		_Utils_Tuple2('ζ裏をζ返せばアルコールでζζ頭を冷やす', 'うらをかえせばあるこーるであたまをひやす'),
		_Utils_Tuple2('ζ要ζζ領を得ないことわざでζ対ζ策', 'ようりょうをえないことわざでたいさく'),
		_Utils_Tuple2('ζ弘ζ法ζ筆をζ選ばず、つむじを曲げる', 'こうぼうふでをえらばず、つむじをまげる'),
		_Utils_Tuple2('ζ耳をζζ疑うζζζ褌を締めるステージ', 'みみをうたがうふんどしをしめるすてーじ'),
		_Utils_Tuple2('ζζ頭をζ絞るリサイクルワールドカップ', 'あたまをしぼるりさいくるわーるどかっぷ'),
		_Utils_Tuple2('ぐうの音も出ないζ綿ζ密なζ公ζ然の秘ζ密', 'ぐうのねもでないめんみつなこうぜんのひみつ'),
		_Utils_Tuple2('ζ眉にζ唾をつける血の気がζ多いζ清ζ潔', 'まゆにつばをつけるちのけがおおいせいけつ'),
		_Utils_Tuple2('奇をてらうプリンターにζ端をζ発する', 'きをてらうぷりんたーにたんをはっする'),
		_Utils_Tuple2('ζ採ζ算が取れるζ鬼にζ金ζ棒のリスト', 'さいさんがとれるおににかなぼうのりすと'),
		_Utils_Tuple2('ζ所ζ帯を持つマカロニとちくわのζ成ζ功', 'しょたいをもつまかろにとちくわのせいこう'),
		_Utils_Tuple2('意地でもおんぶに抱っこでζ大なたを振るう', 'いじでもおんぶにだっこでおおなたをふるう'),
		_Utils_Tuple2('駆け出しアナウンサーにおζζ灸を据える', 'かけだしあなうんさーにおきゅうをすえる'),
		_Utils_Tuple2('ζ弱音を吐くディスカウントにζζ心がζ弾む', 'よわねをはくでぃすかうんとにこころがはずむ'),
		_Utils_Tuple2('科ζ学一ζζ点張りにζ軍ζ配が上がる', 'かがくいってんばりにぐんばいがあがる'),
		_Utils_Tuple2('ζζ直ζ接ハムでζζ帳ζ尻を合わせる背ζ中だ', 'ちょくせつはむでちょうじりをあわせるせなかだ'),
		_Utils_Tuple2('ζ喉から手が出るにおいにζ敷居をまたぐ', 'のどからてがでるにおいにしきいをまたぐ'),
		_Utils_Tuple2('カエルの子はカエルζζ冥利に尽きるお手上げ', 'かえるのこはかえるみょうりにつきるおてあげ'),
		_Utils_Tuple2('あうんの呼ζζ吸マニアがζζ錦をζ飾る', 'あうんのこきゅうまにあがにしきをかざる'),
		_Utils_Tuple2('ζ薄気味ζ悪いもぬけのζ殻のζ倉庫でζ迷う', 'うすきみわるいもぬけのからのそうこでまよう'),
		_Utils_Tuple2('一ζζ刻をζζ争うウハウハにζ思いを焦がす', 'いっこくをあらそううはうはにおもいをこがす'),
		_Utils_Tuple2('ζ横ζ道にそれるアニメがζ後を絶たない', 'よこみちにそれるあにめがあとをたたない'),
		_Utils_Tuple2('当たりζ障りがないζ禁止がζ影を落とす', 'あたりさわりがないきんしがかげをおとす'),
		_Utils_Tuple2('言うだけ野暮にζ声を振りζ絞るスタイル', 'いうだけやぼにこえをふりしぼるすたいる'),
		_Utils_Tuple2('破ζ竹のζζ勢いで目からζζ鱗が落ちる', 'はちくのいきおいでめからうろこがおちる'),
		_Utils_Tuple2('歯にζ衣着せぬ御ζ託をζ並べるバンド', 'はにきぬきせぬごたくをならべるばんど'),
		_Utils_Tuple2('つかみζζ所がないなまめかしい掘り出しζ物', 'つかみどころがないなまめかしいほりだしもの'),
		_Utils_Tuple2('ζζ常軌をζ逸するζ七ζ転び八起きの日ζ誌', 'じょうきをいっするななころびやおきのにっし'),
		_Utils_Tuple2('目にも留まらぬζ能ζ天気がζ金科ζζ玉ζζ条', 'めにもとまらぬのうてんきがきんかぎょくじょう')
	]);
var $author$project$Page$TypeShortWord$testDataOfShowtWords = {
	id: -1,
	title: 'title',
	words: A2(
		$elm$core$List$map,
		function (x) {
			return A2($author$project$Data$ShortWord$Word, x.a, x.b);
		},
		$author$project$Emb$ShortWord1$list)
};
var $author$project$Page$TypeShortWord$newTestReadyData = A7(
	$author$project$Page$TypeShortWord$ReadyData,
	$author$project$Page$TypeShortWord$Trial,
	$author$project$Page$TypeShortWord$testDataOfShowtWords,
	$author$project$Page$TypeShortWord$initCustomTypingWords($author$project$Page$TypeShortWord$testDataOfShowtWords.words),
	$author$project$Page$TypeShortWord$initialCountDownTimer,
	$elm$core$Maybe$Nothing,
	_List_Nil,
	$elm$core$Maybe$Nothing);
var $author$project$Page$TypeShortWord$Top = function (a) {
	return {$: 'Top', a: a};
};
var $author$project$Page$TypeShortWord$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Page$TypeShortWord$Top,
			A2(
				$elm$url$Url$Parser$questionMark,
				$elm$url$Url$Parser$top,
				$elm$url$Url$Parser$Query$int('id')))
		]));
var $author$project$Page$TypeShortWord$ShuffleWords = function (a) {
	return {$: 'ShuffleWords', a: a};
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
			});
	});
var $owanturist$elm_union_find$UnionFind$findFast = F2(
	function (id, dict) {
		findFast:
		while (true) {
			var _v0 = A2($elm$core$Dict$get, id, dict);
			if (_v0.$ === 'Nothing') {
				return id;
			} else {
				var cursor = _v0.a;
				if (_Utils_eq(id, cursor)) {
					return id;
				} else {
					var $temp$id = cursor,
						$temp$dict = dict;
					id = $temp$id;
					dict = $temp$dict;
					continue findFast;
				}
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$find = F2(
	function (id, _v0) {
		var dict = _v0.b;
		return A2($owanturist$elm_union_find$UnionFind$findFast, id, dict);
	});
var $elm$core$Array$isEmpty = function (_v0) {
	var len = _v0.a;
	return !len;
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $owanturist$elm_union_find$UnionFind$QuickUnionPathCompression = F2(
	function (a, b) {
		return {$: 'QuickUnionPathCompression', a: a, b: b};
	});
var $owanturist$elm_union_find$UnionFind$quickUnionPathCompression = A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, 0, $elm$core$Dict$empty);
var $owanturist$elm_union_find$UnionFind$findCompressed = F2(
	function (id, dict) {
		var _v0 = A2($elm$core$Dict$get, id, dict);
		if (_v0.$ === 'Nothing') {
			return _Utils_Tuple2(
				id,
				A3($elm$core$Dict$insert, id, id, dict));
		} else {
			var cursor = _v0.a;
			if (_Utils_eq(id, cursor)) {
				return _Utils_Tuple2(id, dict);
			} else {
				var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, cursor, dict);
				var parent = _v1.a;
				var nextDict = _v1.b;
				return _Utils_Tuple2(
					parent,
					A3($elm$core$Dict$insert, id, parent, nextDict));
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$union = F3(
	function (left, right, _v0) {
		var count_ = _v0.a;
		var dict = _v0.b;
		var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, left, dict);
		var leftRoot = _v1.a;
		var leftDict = _v1.b;
		var _v2 = A2($owanturist$elm_union_find$UnionFind$findCompressed, right, leftDict);
		var rightRoot = _v2.a;
		var rightDict = _v2.b;
		return _Utils_eq(leftRoot, rightRoot) ? A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, count_, rightDict) : A2(
			$owanturist$elm_union_find$UnionFind$QuickUnionPathCompression,
			count_ + 1,
			A3($elm$core$Dict$insert, leftRoot, rightRoot, rightDict));
	});
var $elm_community$random_extra$Utils$selectUniqByIndexes = F2(
	function (values, randomIndexes) {
		var modByLength = $elm$core$Basics$modBy(
			$elm$core$Array$length(values));
		var step = F2(
			function (randomIndex, _v1) {
				var uf = _v1.a;
				var acc = _v1.b;
				var leaderOfElement = A2($owanturist$elm_union_find$UnionFind$find, randomIndex, uf);
				var leaderOfNextElement = A2(
					$owanturist$elm_union_find$UnionFind$find,
					modByLength(leaderOfElement + 1),
					uf);
				var _v0 = A2($elm$core$Array$get, leaderOfElement, values);
				if (_v0.$ === 'Nothing') {
					return _Utils_Tuple2(uf, acc);
				} else {
					var value = _v0.a;
					return _Utils_Tuple2(
						A3($owanturist$elm_union_find$UnionFind$union, leaderOfElement, leaderOfNextElement, uf),
						A2($elm$core$List$cons, value, acc));
				}
			});
		return $elm$core$Array$isEmpty(values) ? _List_Nil : A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2($owanturist$elm_union_find$UnionFind$quickUnionPathCompression, _List_Nil),
			randomIndexes).b;
	});
var $elm_community$random_extra$Random$List$shuffle = function (list) {
	var values = $elm$core$Array$fromList(list);
	var length = $elm$core$Array$length(values);
	return A2(
		$elm$random$Random$map,
		$elm_community$random_extra$Utils$selectUniqByIndexes(values),
		A2(
			$elm$random$Random$list,
			length,
			A2($elm$random$Random$int, 0, length - 1)));
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Page$TypeShortWord$shuffleWords = function (shortWords) {
	return A2(
		$elm$random$Random$generate,
		$author$project$Page$TypeShortWord$ShuffleWords,
		A2(
			$elm$random$Random$map,
			$elm$core$List$take(
				A2(
					$elm$core$Basics$min,
					20,
					$elm$core$List$length(shortWords.words))),
			$elm_community$random_extra$Random$List$shuffle(shortWords.words)));
};
var $author$project$Page$TypeShortWord$init = function (env) {
	var url = env.url;
	var readyData = $author$project$Page$TypeShortWord$newTestReadyData;
	var id = A2(
		$elm$core$Maybe$map,
		function (_v0) {
			var d1 = _v0.a;
			return d1;
		},
		A2(
			$elm$url$Url$Parser$parse,
			$author$project$Page$TypeShortWord$routeParser,
			_Utils_update(
				url,
				{path: '/'})));
	return _Utils_Tuple2(
		A2(
			$author$project$Page$TypeShortWord$Model,
			$author$project$Page$TypeShortWord$Ready(readyData),
			''),
		$author$project$Page$TypeShortWord$shuffleWords(readyData.shortWords));
};
var $author$project$Page$User$Init = {$: 'Init'};
var $author$project$Page$User$Model = F2(
	function (state, user) {
		return {state: state, user: user};
	});
var $author$project$Page$User$init = function (env) {
	return _Utils_Tuple2(
		A2($author$project$Page$User$Model, $author$project$Page$User$Init, env.user),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Page$WordList$Init = {$: 'Init'};
var $author$project$Page$WordList$Model = function (state) {
	return {state: state};
};
var $author$project$Page$WordList$ReceiveAllWordList = function (a) {
	return {$: 'ReceiveAllWordList', a: a};
};
var $author$project$API$WordSummary = F3(
	function (id, title, userName) {
		return {id: id, title: title, userName: userName};
	});
var $author$project$API$wordSummaryDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$API$WordSummary,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'userName', $elm$json$Json$Decode$string));
var $author$project$API$wordListDecoder = $elm$json$Json$Decode$list($author$project$API$wordSummaryDecoder);
var $author$project$API$getAllWordList = function (msg) {
	return $elm$http$Http$get(
		{
			expect: A2($elm$http$Http$expectJson, msg, $author$project$API$wordListDecoder),
			url: $author$project$API$host + ($author$project$API$api + '?getAllWordList')
		});
};
var $author$project$Page$WordList$init = function (env) {
	return _Utils_Tuple2(
		$author$project$Page$WordList$Model($author$project$Page$WordList$Init),
		$author$project$API$getAllWordList($author$project$Page$WordList$ReceiveAllWordList));
};
var $author$project$Main$goTo = F2(
	function (url, model) {
		var pEnv = model.env;
		var newEnv = _Utils_update(
			pEnv,
			{url: url});
		var newModel = _Utils_update(
			model,
			{env: newEnv});
		var _v0 = newEnv.url.fragment;
		if (_v0.$ === 'Just') {
			switch (_v0.a) {
				case 'post':
					var _v1 = function () {
						var _v2 = model.postLongWordPageCache;
						if (_v2.$ === 'Just') {
							var cm = _v2.a;
							return _Utils_Tuple2(cm, $elm$core$Platform$Cmd$none);
						} else {
							return $author$project$Page$PostLongWord$init(newEnv);
						}
					}();
					var pageModel = _v1.a;
					var pageCmd = _v1.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$PostLongWordPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$PostLongWordMsg, pageCmd));
				case 'user':
					var _v3 = $author$project$Page$User$init(newEnv);
					var pageModel = _v3.a;
					var pageCmd = _v3.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$UserPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$UserMsg, pageCmd));
				case 'list':
					var _v4 = $author$project$Page$WordList$init(newEnv);
					var pageModel = _v4.a;
					var pageCmd = _v4.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$WordListPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$WordListMsg, pageCmd));
				case 'typeLongWord':
					var _v5 = $author$project$Page$TypeLongWord$init(newEnv);
					var pageModel = _v5.a;
					var pageCmd = _v5.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$TypeLongWordPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeLongWordMsg, pageCmd));
				case 'etc':
					var _v6 = $author$project$Page$Etc$init(newEnv);
					var pageModel = _v6.a;
					var pageCmd = _v6.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$EtcPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$EtcMsg, pageCmd));
				case 'typeShortWord':
					var _v7 = $author$project$Page$TypeShortWord$init(newEnv);
					var pageModel = _v7.a;
					var pageCmd = _v7.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$TypeShortWordPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeShortWordMsg, pageCmd));
				case 'typeAny':
					var _v8 = $author$project$Page$TypeAny$init(newEnv);
					var pageModel = _v8.a;
					var pageCmd = _v8.b;
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{
								page: $author$project$Main$TypeAnyPage(pageModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeAnyMsg, pageCmd));
				default:
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		} else {
			var _v9 = $author$project$Page$TypeLongWord$init(newEnv);
			var pageModel = _v9.a;
			var pageCmd = _v9.b;
			return _Utils_Tuple2(
				_Utils_update(
					newModel,
					{
						page: $author$project$Main$TypeLongWordPage(pageModel)
					}),
				A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeLongWordMsg, pageCmd));
		}
	});
var $author$project$Data$User$new = {id: 'nothing', name: 'nothing'};
var $author$project$Main$init = F3(
	function (flags, url, key) {
		var env = {key: key, url: url, user: $author$project$Data$User$new};
		var model = {env: env, page: $author$project$Main$NotFound, postLongWordPageCache: $elm$core$Maybe$Nothing};
		var _v0 = A2($author$project$Main$goTo, url, model);
		var m = _v0.a;
		var c = _v0.b;
		return _Utils_Tuple2(
			m,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						c,
						$author$project$Ports$getUser(_Utils_Tuple0)
					])));
	});
var $author$project$Main$RestoreUser = function (a) {
	return {$: 'RestoreUser', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Ports$restoreUser = _Platform_incomingPort('restoreUser', $elm$json$Json$Decode$value);
var $author$project$Page$PostLongWord$TypeLongWordMsg = function (a) {
	return {$: 'TypeLongWordMsg', a: a};
};
var $author$project$Page$TypeLongWord$KeyDown = function (a) {
	return {$: 'KeyDown', a: a};
};
var $author$project$Page$TypeLongWord$keyDecoder = A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string);
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $author$project$Page$TypeLongWord$subscriptions = $elm$core$Platform$Sub$batch(
	_List_fromArray(
		[
			$elm$browser$Browser$Events$onKeyDown(
			A2($elm$json$Json$Decode$map, $author$project$Page$TypeLongWord$KeyDown, $author$project$Page$TypeLongWord$keyDecoder))
		]));
var $author$project$Page$PostLongWord$subscriptions = function (model) {
	var s1 = function () {
		var _v0 = model.state;
		if (_v0.$ === 'Trial') {
			var subModel = _v0.a;
			return A2($elm$core$Platform$Sub$map, $author$project$Page$PostLongWord$TypeLongWordMsg, $author$project$Page$TypeLongWord$subscriptions);
		} else {
			return $elm$core$Platform$Sub$none;
		}
	}();
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[s1]));
};
var $author$project$Page$TypeAny$KeyDown = function (a) {
	return {$: 'KeyDown', a: a};
};
var $author$project$Page$TypeAny$OneSecond = function (a) {
	return {$: 'OneSecond', a: a};
};
var $author$project$Page$TypeAny$TenSecond = function (a) {
	return {$: 'TenSecond', a: a};
};
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $author$project$Page$TypeAny$keyDecoder = A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string);
var $author$project$Page$TypeAny$subscriptions = $elm$core$Platform$Sub$batch(
	_List_fromArray(
		[
			$elm$browser$Browser$Events$onKeyDown(
			A2($elm$json$Json$Decode$map, $author$project$Page$TypeAny$KeyDown, $author$project$Page$TypeAny$keyDecoder)),
			A2($elm$time$Time$every, 10000, $author$project$Page$TypeAny$TenSecond),
			A2($elm$time$Time$every, 1000, $author$project$Page$TypeAny$OneSecond)
		]));
var $author$project$Page$TypeShortWord$KeyDown = function (a) {
	return {$: 'KeyDown', a: a};
};
var $author$project$Page$TypeShortWord$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $author$project$Page$TypeShortWord$TickRhythm = function (a) {
	return {$: 'TickRhythm', a: a};
};
var $author$project$Page$TypeShortWord$getCountDownTimer = function (model) {
	var _v0 = model.state;
	switch (_v0.$) {
		case 'Init':
			return $elm$core$Maybe$Nothing;
		case 'Error':
			return $elm$core$Maybe$Nothing;
		default:
			var readyData = _v0.a;
			return $elm$core$Maybe$Just(readyData.countDownTimer);
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Page$TypeShortWord$getCurrentWord = function (words) {
	var _v0 = $elm$core$List$head(words.rest);
	if (_v0.$ === 'Nothing') {
		return $elm$core$Maybe$Nothing;
	} else {
		var customTypingWord = _v0.a;
		return $elm$core$Maybe$Just(customTypingWord);
	}
};
var $author$project$Page$TypeShortWord$getRhythmTimer = function (model) {
	var _v0 = model.state;
	switch (_v0.$) {
		case 'Init':
			return $elm$core$Maybe$Nothing;
		case 'Error':
			return $elm$core$Maybe$Nothing;
		default:
			var readyData = _v0.a;
			var _v1 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
			if (_v1.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var word = _v1.a;
				return $elm$core$Maybe$Just(word.rhythmTimer);
			}
	}
};
var $author$project$CountDown$Error = {$: 'Error'};
var $author$project$CountDown$Ready = {$: 'Ready'};
var $author$project$CountDown$Stop = {$: 'Stop'};
var $author$project$CountDown$Tick = {$: 'Tick'};
var $author$project$CountDown$Zero = {$: 'Zero'};
var $author$project$CountDown$getState = function (_v0) {
	var timer = _v0.a;
	var _v1 = timer.start;
	if (_v1) {
		return (timer.time <= 0) ? $author$project$CountDown$Zero : $author$project$CountDown$Tick;
	} else {
		return (timer.time <= 0) ? $author$project$CountDown$Zero : (((0 < timer.time) && (_Utils_cmp(timer.time, timer.initialTime) < 0)) ? $author$project$CountDown$Stop : (_Utils_eq(timer.time, timer.initialTime) ? $author$project$CountDown$Ready : $author$project$CountDown$Error));
	}
};
var $author$project$Page$TypeShortWord$keyDecoder = A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string);
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$CountDown$tick = function (_v0) {
	var timer = _v0.a;
	var newTime = ((timer.time - timer.interval) < 0) ? 0 : (timer.time - timer.interval);
	var newStart = (!newTime) ? false : true;
	return $author$project$CountDown$Timer(
		_Utils_update(
			timer,
			{start: newStart, time: newTime}));
};
var $author$project$CountDown$treatSubscription = F2(
	function (_v0, userMsg) {
		var timer = _v0.a;
		if (A2(
			$elm$core$List$member,
			$author$project$CountDown$getState(
				$author$project$CountDown$Timer(timer)),
			_List_fromArray(
				[$author$project$CountDown$Zero, $author$project$CountDown$Stop, $author$project$CountDown$Ready]))) {
			return $elm$core$Platform$Sub$none;
		} else {
			var subTick = F2(
				function (subTimer, time) {
					return $author$project$CountDown$tick(subTimer);
				});
			var sub = A2(
				$elm$time$Time$every,
				timer.interval,
				subTick(
					$author$project$CountDown$Timer(timer)));
			return A2($elm$core$Platform$Sub$map, userMsg, sub);
		}
	});
var $author$project$Page$TypeShortWord$subscriptions = function (model) {
	var rt = $author$project$Page$TypeShortWord$getRhythmTimer(model);
	var ct = $author$project$Page$TypeShortWord$getCountDownTimer(model);
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onKeyDown(
				A2($elm$json$Json$Decode$map, $author$project$Page$TypeShortWord$KeyDown, $author$project$Page$TypeShortWord$keyDecoder)),
				function () {
				if (ct.$ === 'Nothing') {
					return $elm$core$Platform$Sub$none;
				} else {
					var jct = ct.a;
					var _v1 = _Utils_Tuple2(
						$author$project$CountDown$getState(jct),
						rt);
					if ((_v1.a.$ === 'Zero') && (_v1.b.$ === 'Just')) {
						var _v2 = _v1.a;
						var jrt = _v1.b.a;
						return A2($author$project$CountDown$treatSubscription, jrt, $author$project$Page$TypeShortWord$TickRhythm);
					} else {
						return A2($author$project$CountDown$treatSubscription, jct, $author$project$Page$TypeShortWord$Tick);
					}
				}
			}()
			]));
};
var $author$project$Main$subscriptions = function (model) {
	var s2 = $author$project$Ports$restoreUser($author$project$Main$RestoreUser);
	var s1 = function () {
		var _v0 = model.page;
		switch (_v0.$) {
			case 'TypeLongWordPage':
				var subModel = _v0.a;
				return A2($elm$core$Platform$Sub$map, $author$project$Main$TypeLongWordMsg, $author$project$Page$TypeLongWord$subscriptions);
			case 'PostLongWordPage':
				var subModel = _v0.a;
				return A2(
					$elm$core$Platform$Sub$map,
					$author$project$Main$PostLongWordMsg,
					$author$project$Page$PostLongWord$subscriptions(subModel));
			case 'TypeAnyPage':
				var subModel = _v0.a;
				return A2($elm$core$Platform$Sub$map, $author$project$Main$TypeAnyMsg, $author$project$Page$TypeAny$subscriptions);
			case 'TypeShortWordPage':
				var subModel = _v0.a;
				return A2(
					$elm$core$Platform$Sub$map,
					$author$project$Main$TypeShortWordMsg,
					$author$project$Page$TypeShortWord$subscriptions(subModel));
			default:
				return $elm$core$Platform$Sub$none;
		}
	}();
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[s1, s2]));
};
var $author$project$Main$ReceiveNewUserID = function (a) {
	return {$: 'ReceiveNewUserID', a: a};
};
var $author$project$Main$TopMsg = function (a) {
	return {$: 'TopMsg', a: a};
};
var $author$project$Main$TopPage = function (a) {
	return {$: 'TopPage', a: a};
};
var $author$project$Data$User$User = F2(
	function (id, name) {
		return {id: id, name: name};
	});
var $elm$core$String$trim = _String_trim;
var $author$project$Data$User$trim = function (user) {
	return {
		id: $elm$core$String$trim(user.id),
		name: $elm$core$String$trim(user.name)
	};
};
var $author$project$Data$User$encodeUser = function (rUser) {
	var user = $author$project$Data$User$trim(rUser);
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$string(user.id)),
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(user.name))
			]));
};
var $author$project$Data$User$userDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Data$User$User,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string));
var $author$project$API$getNewUserID = function (msg) {
	return $elm$http$Http$get(
		{
			expect: A2($elm$http$Http$expectJson, msg, $author$project$Data$User$userDecoder),
			url: $author$project$API$host + ($author$project$API$api + '?getNewID')
		});
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $author$project$Ports$saveUser = _Platform_outgoingPort('saveUser', $elm$core$Basics$identity);
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Page$Etc$update = F3(
	function (msg, model, env) {
		return _Utils_Tuple3(
			_Utils_update(
				model,
				{state: $author$project$Page$Etc$Init}),
			$elm$core$Platform$Cmd$none,
			env);
	});
var $author$project$Page$PostLongWord$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Page$PostLongWord$ReceivePost = function (a) {
	return {$: 'ReceivePost', a: a};
};
var $author$project$Page$PostLongWord$Success = function (a) {
	return {$: 'Success', a: a};
};
var $author$project$Page$PostLongWord$Trial = function (a) {
	return {$: 'Trial', a: a};
};
var $author$project$Page$PostLongWord$Waiting = function (a) {
	return {$: 'Waiting', a: a};
};
var $author$project$Page$TypeLongWord$Trial = function (a) {
	return {$: 'Trial', a: a};
};
var $author$project$Typing1$Data = F3(
	function (fixedWords, restWords, state) {
		return {fixedWords: fixedWords, restWords: restWords, state: state};
	});
var $author$project$Typing1$State = F3(
	function (inputBuffer, tmpFixed, candidates) {
		return {candidates: candidates, inputBuffer: inputBuffer, tmpFixed: tmpFixed};
	});
var $author$project$Typing1$Rule = F2(
	function (input, output) {
		return {input: input, output: output};
	});
var $author$project$Typing1$romanTable = _List_fromArray(
	[
		A2($author$project$Typing1$Rule, '-', 'ー'),
		A2($author$project$Typing1$Rule, '~', '〜'),
		A2($author$project$Typing1$Rule, '.', '。'),
		A2($author$project$Typing1$Rule, ',', '、'),
		A2($author$project$Typing1$Rule, '[', '「'),
		A2($author$project$Typing1$Rule, ']', '」'),
		A2($author$project$Typing1$Rule, 'va', 'ゔぁ'),
		A2($author$project$Typing1$Rule, 'vi', 'ゔぃ'),
		A2($author$project$Typing1$Rule, 'vu', 'ゔ'),
		A2($author$project$Typing1$Rule, 've', 'ゔぇ'),
		A2($author$project$Typing1$Rule, 'vo', 'ゔぉ'),
		A2($author$project$Typing1$Rule, 'vya', 'ゔゃ'),
		A2($author$project$Typing1$Rule, 'vyi', 'ゔぃ'),
		A2($author$project$Typing1$Rule, 'vyu', 'ゔゅ'),
		A2($author$project$Typing1$Rule, 'vye', 'ゔぇ'),
		A2($author$project$Typing1$Rule, 'vyo', 'ゔょ'),
		A2($author$project$Typing1$Rule, 'kya', 'きゃ'),
		A2($author$project$Typing1$Rule, 'kyi', 'きぃ'),
		A2($author$project$Typing1$Rule, 'kyu', 'きゅ'),
		A2($author$project$Typing1$Rule, 'kye', 'きぇ'),
		A2($author$project$Typing1$Rule, 'kyo', 'きょ'),
		A2($author$project$Typing1$Rule, 'gya', 'ぎゃ'),
		A2($author$project$Typing1$Rule, 'gyi', 'ぎぃ'),
		A2($author$project$Typing1$Rule, 'gyu', 'ぎゅ'),
		A2($author$project$Typing1$Rule, 'gye', 'ぎぇ'),
		A2($author$project$Typing1$Rule, 'gyo', 'ぎょ'),
		A2($author$project$Typing1$Rule, 'sya', 'しゃ'),
		A2($author$project$Typing1$Rule, 'syi', 'しぃ'),
		A2($author$project$Typing1$Rule, 'syu', 'しゅ'),
		A2($author$project$Typing1$Rule, 'sye', 'しぇ'),
		A2($author$project$Typing1$Rule, 'syo', 'しょ'),
		A2($author$project$Typing1$Rule, 'sha', 'しゃ'),
		A2($author$project$Typing1$Rule, 'shi', 'し'),
		A2($author$project$Typing1$Rule, 'shu', 'しゅ'),
		A2($author$project$Typing1$Rule, 'she', 'しぇ'),
		A2($author$project$Typing1$Rule, 'sho', 'しょ'),
		A2($author$project$Typing1$Rule, 'zya', 'じゃ'),
		A2($author$project$Typing1$Rule, 'zyi', 'じぃ'),
		A2($author$project$Typing1$Rule, 'zyu', 'じゅ'),
		A2($author$project$Typing1$Rule, 'zye', 'じぇ'),
		A2($author$project$Typing1$Rule, 'zyo', 'じょ'),
		A2($author$project$Typing1$Rule, 'tya', 'ちゃ'),
		A2($author$project$Typing1$Rule, 'tyi', 'ちぃ'),
		A2($author$project$Typing1$Rule, 'tyu', 'ちゅ'),
		A2($author$project$Typing1$Rule, 'tye', 'ちぇ'),
		A2($author$project$Typing1$Rule, 'tyo', 'ちょ'),
		A2($author$project$Typing1$Rule, 'cha', 'ちゃ'),
		A2($author$project$Typing1$Rule, 'chi', 'ち'),
		A2($author$project$Typing1$Rule, 'chu', 'ちゅ'),
		A2($author$project$Typing1$Rule, 'che', 'ちぇ'),
		A2($author$project$Typing1$Rule, 'cho', 'ちょ'),
		A2($author$project$Typing1$Rule, 'cya', 'ちゃ'),
		A2($author$project$Typing1$Rule, 'cyi', 'ちぃ'),
		A2($author$project$Typing1$Rule, 'cyu', 'ちゅ'),
		A2($author$project$Typing1$Rule, 'cye', 'ちぇ'),
		A2($author$project$Typing1$Rule, 'cyo', 'ちょ'),
		A2($author$project$Typing1$Rule, 'dya', 'ぢゃ'),
		A2($author$project$Typing1$Rule, 'dyi', 'ぢぃ'),
		A2($author$project$Typing1$Rule, 'dyu', 'ぢゅ'),
		A2($author$project$Typing1$Rule, 'dye', 'ぢぇ'),
		A2($author$project$Typing1$Rule, 'dyo', 'ぢょ'),
		A2($author$project$Typing1$Rule, 'tsa', 'つぁ'),
		A2($author$project$Typing1$Rule, 'tsi', 'つぃ'),
		A2($author$project$Typing1$Rule, 'tse', 'つぇ'),
		A2($author$project$Typing1$Rule, 'tso', 'つぉ'),
		A2($author$project$Typing1$Rule, 'tha', 'てゃ'),
		A2($author$project$Typing1$Rule, 'thi', 'てぃ'),
		A2($author$project$Typing1$Rule, 'thu', 'てゅ'),
		A2($author$project$Typing1$Rule, 'the', 'てぇ'),
		A2($author$project$Typing1$Rule, 'tho', 'てょ'),
		A2($author$project$Typing1$Rule, 'dha', 'でゃ'),
		A2($author$project$Typing1$Rule, 'dhi', 'でぃ'),
		A2($author$project$Typing1$Rule, 'dhu', 'でゅ'),
		A2($author$project$Typing1$Rule, 'dhe', 'でぇ'),
		A2($author$project$Typing1$Rule, 'dho', 'でょ'),
		A2($author$project$Typing1$Rule, 'twa', 'とぁ'),
		A2($author$project$Typing1$Rule, 'twi', 'とぃ'),
		A2($author$project$Typing1$Rule, 'twu', 'とぅ'),
		A2($author$project$Typing1$Rule, 'twe', 'とぇ'),
		A2($author$project$Typing1$Rule, 'two', 'とぉ'),
		A2($author$project$Typing1$Rule, 'dwa', 'どぁ'),
		A2($author$project$Typing1$Rule, 'dwi', 'どぃ'),
		A2($author$project$Typing1$Rule, 'dwu', 'どぅ'),
		A2($author$project$Typing1$Rule, 'dwe', 'どぇ'),
		A2($author$project$Typing1$Rule, 'dwo', 'どぉ'),
		A2($author$project$Typing1$Rule, 'nya', 'にゃ'),
		A2($author$project$Typing1$Rule, 'nyi', 'にぃ'),
		A2($author$project$Typing1$Rule, 'nyu', 'にゅ'),
		A2($author$project$Typing1$Rule, 'nye', 'にぇ'),
		A2($author$project$Typing1$Rule, 'nyo', 'にょ'),
		A2($author$project$Typing1$Rule, 'hya', 'ひゃ'),
		A2($author$project$Typing1$Rule, 'hyi', 'ひぃ'),
		A2($author$project$Typing1$Rule, 'hyu', 'ひゅ'),
		A2($author$project$Typing1$Rule, 'hye', 'ひぇ'),
		A2($author$project$Typing1$Rule, 'hyo', 'ひょ'),
		A2($author$project$Typing1$Rule, 'bya', 'びゃ'),
		A2($author$project$Typing1$Rule, 'byi', 'びぃ'),
		A2($author$project$Typing1$Rule, 'byu', 'びゅ'),
		A2($author$project$Typing1$Rule, 'bye', 'びぇ'),
		A2($author$project$Typing1$Rule, 'byo', 'びょ'),
		A2($author$project$Typing1$Rule, 'pya', 'ぴゃ'),
		A2($author$project$Typing1$Rule, 'pyi', 'ぴぃ'),
		A2($author$project$Typing1$Rule, 'pyu', 'ぴゅ'),
		A2($author$project$Typing1$Rule, 'pye', 'ぴぇ'),
		A2($author$project$Typing1$Rule, 'pyo', 'ぴょ'),
		A2($author$project$Typing1$Rule, 'fa', 'ふぁ'),
		A2($author$project$Typing1$Rule, 'fi', 'ふぃ'),
		A2($author$project$Typing1$Rule, 'fu', 'ふ'),
		A2($author$project$Typing1$Rule, 'fe', 'ふぇ'),
		A2($author$project$Typing1$Rule, 'fo', 'ふぉ'),
		A2($author$project$Typing1$Rule, 'fya', 'ふゃ'),
		A2($author$project$Typing1$Rule, 'fyu', 'ふゅ'),
		A2($author$project$Typing1$Rule, 'fyo', 'ふょ'),
		A2($author$project$Typing1$Rule, 'mya', 'みゃ'),
		A2($author$project$Typing1$Rule, 'myi', 'みぃ'),
		A2($author$project$Typing1$Rule, 'myu', 'みゅ'),
		A2($author$project$Typing1$Rule, 'mye', 'みぇ'),
		A2($author$project$Typing1$Rule, 'myo', 'みょ'),
		A2($author$project$Typing1$Rule, 'rya', 'りゃ'),
		A2($author$project$Typing1$Rule, 'ryi', 'りぃ'),
		A2($author$project$Typing1$Rule, 'ryu', 'りゅ'),
		A2($author$project$Typing1$Rule, 'rye', 'りぇ'),
		A2($author$project$Typing1$Rule, 'ryo', 'りょ'),
		A2($author$project$Typing1$Rule, 'nn', 'ん'),
		A2($author$project$Typing1$Rule, 'n', 'ん'),
		A2($author$project$Typing1$Rule, 'xn', 'ん'),
		A2($author$project$Typing1$Rule, 'a', 'あ'),
		A2($author$project$Typing1$Rule, 'i', 'い'),
		A2($author$project$Typing1$Rule, 'u', 'う'),
		A2($author$project$Typing1$Rule, 'wu', 'う'),
		A2($author$project$Typing1$Rule, 'e', 'え'),
		A2($author$project$Typing1$Rule, 'o', 'お'),
		A2($author$project$Typing1$Rule, 'xa', 'ぁ'),
		A2($author$project$Typing1$Rule, 'xi', 'ぃ'),
		A2($author$project$Typing1$Rule, 'xu', 'ぅ'),
		A2($author$project$Typing1$Rule, 'xe', 'ぇ'),
		A2($author$project$Typing1$Rule, 'xo', 'ぉ'),
		A2($author$project$Typing1$Rule, 'la', 'ぁ'),
		A2($author$project$Typing1$Rule, 'li', 'ぃ'),
		A2($author$project$Typing1$Rule, 'lu', 'ぅ'),
		A2($author$project$Typing1$Rule, 'le', 'ぇ'),
		A2($author$project$Typing1$Rule, 'lo', 'ぉ'),
		A2($author$project$Typing1$Rule, 'lyi', 'ぃ'),
		A2($author$project$Typing1$Rule, 'xyi', 'ぃ'),
		A2($author$project$Typing1$Rule, 'lye', 'ぇ'),
		A2($author$project$Typing1$Rule, 'xye', 'ぇ'),
		A2($author$project$Typing1$Rule, 'ye', 'いぇ'),
		A2($author$project$Typing1$Rule, 'ka', 'か'),
		A2($author$project$Typing1$Rule, 'ki', 'き'),
		A2($author$project$Typing1$Rule, 'ku', 'く'),
		A2($author$project$Typing1$Rule, 'ke', 'け'),
		A2($author$project$Typing1$Rule, 'ko', 'こ'),
		A2($author$project$Typing1$Rule, 'xka', 'ヵ'),
		A2($author$project$Typing1$Rule, 'xke', 'ヶ'),
		A2($author$project$Typing1$Rule, 'lka', 'ヵ'),
		A2($author$project$Typing1$Rule, 'lke', 'ヶ'),
		A2($author$project$Typing1$Rule, 'ga', 'が'),
		A2($author$project$Typing1$Rule, 'gi', 'ぎ'),
		A2($author$project$Typing1$Rule, 'gu', 'ぐ'),
		A2($author$project$Typing1$Rule, 'ge', 'げ'),
		A2($author$project$Typing1$Rule, 'go', 'ご'),
		A2($author$project$Typing1$Rule, 'sa', 'さ'),
		A2($author$project$Typing1$Rule, 'si', 'し'),
		A2($author$project$Typing1$Rule, 'su', 'す'),
		A2($author$project$Typing1$Rule, 'se', 'せ'),
		A2($author$project$Typing1$Rule, 'so', 'そ'),
		A2($author$project$Typing1$Rule, 'ca', 'か'),
		A2($author$project$Typing1$Rule, 'ci', 'し'),
		A2($author$project$Typing1$Rule, 'cu', 'く'),
		A2($author$project$Typing1$Rule, 'ce', 'せ'),
		A2($author$project$Typing1$Rule, 'co', 'こ'),
		A2($author$project$Typing1$Rule, 'qa', 'くぁ'),
		A2($author$project$Typing1$Rule, 'qi', 'くぃ'),
		A2($author$project$Typing1$Rule, 'qu', 'く'),
		A2($author$project$Typing1$Rule, 'qe', 'くぇ'),
		A2($author$project$Typing1$Rule, 'qo', 'くぉ'),
		A2($author$project$Typing1$Rule, 'kwa', 'くぁ'),
		A2($author$project$Typing1$Rule, 'gwa', 'ぐぁ'),
		A2($author$project$Typing1$Rule, 'gwi', 'ぐぃ'),
		A2($author$project$Typing1$Rule, 'gwu', 'ぐぅ'),
		A2($author$project$Typing1$Rule, 'gwe', 'ぐぇ'),
		A2($author$project$Typing1$Rule, 'gwo', 'ぐぉ'),
		A2($author$project$Typing1$Rule, 'za', 'ざ'),
		A2($author$project$Typing1$Rule, 'zi', 'じ'),
		A2($author$project$Typing1$Rule, 'zu', 'ず'),
		A2($author$project$Typing1$Rule, 'ze', 'ぜ'),
		A2($author$project$Typing1$Rule, 'zo', 'ぞ'),
		A2($author$project$Typing1$Rule, 'ja', 'じゃ'),
		A2($author$project$Typing1$Rule, 'ji', 'じ'),
		A2($author$project$Typing1$Rule, 'ju', 'じゅ'),
		A2($author$project$Typing1$Rule, 'je', 'じぇ'),
		A2($author$project$Typing1$Rule, 'jo', 'じょ'),
		A2($author$project$Typing1$Rule, 'jya', 'じゃ'),
		A2($author$project$Typing1$Rule, 'jyi', 'じぃ'),
		A2($author$project$Typing1$Rule, 'jyu', 'じゅ'),
		A2($author$project$Typing1$Rule, 'jye', 'じぇ'),
		A2($author$project$Typing1$Rule, 'jyo', 'じょ'),
		A2($author$project$Typing1$Rule, 'ta', 'た'),
		A2($author$project$Typing1$Rule, 'ti', 'ち'),
		A2($author$project$Typing1$Rule, 'tu', 'つ'),
		A2($author$project$Typing1$Rule, 'tsu', 'つ'),
		A2($author$project$Typing1$Rule, 'te', 'て'),
		A2($author$project$Typing1$Rule, 'to', 'と'),
		A2($author$project$Typing1$Rule, 'da', 'だ'),
		A2($author$project$Typing1$Rule, 'di', 'ぢ'),
		A2($author$project$Typing1$Rule, 'du', 'づ'),
		A2($author$project$Typing1$Rule, 'de', 'で'),
		A2($author$project$Typing1$Rule, 'do', 'ど'),
		A2($author$project$Typing1$Rule, 'xtu', 'っ'),
		A2($author$project$Typing1$Rule, 'xtsu', 'っ'),
		A2($author$project$Typing1$Rule, 'ltu', 'っ'),
		A2($author$project$Typing1$Rule, 'ltsu', 'っ'),
		A2($author$project$Typing1$Rule, 'na', 'な'),
		A2($author$project$Typing1$Rule, 'ni', 'に'),
		A2($author$project$Typing1$Rule, 'nu', 'ぬ'),
		A2($author$project$Typing1$Rule, 'ne', 'ね'),
		A2($author$project$Typing1$Rule, 'no', 'の'),
		A2($author$project$Typing1$Rule, 'ha', 'は'),
		A2($author$project$Typing1$Rule, 'hi', 'ひ'),
		A2($author$project$Typing1$Rule, 'hu', 'ふ'),
		A2($author$project$Typing1$Rule, 'fu', 'ふ'),
		A2($author$project$Typing1$Rule, 'he', 'へ'),
		A2($author$project$Typing1$Rule, 'ho', 'ほ'),
		A2($author$project$Typing1$Rule, 'ba', 'ば'),
		A2($author$project$Typing1$Rule, 'bi', 'び'),
		A2($author$project$Typing1$Rule, 'bu', 'ぶ'),
		A2($author$project$Typing1$Rule, 'be', 'べ'),
		A2($author$project$Typing1$Rule, 'bo', 'ぼ'),
		A2($author$project$Typing1$Rule, 'pa', 'ぱ'),
		A2($author$project$Typing1$Rule, 'pi', 'ぴ'),
		A2($author$project$Typing1$Rule, 'pu', 'ぷ'),
		A2($author$project$Typing1$Rule, 'pe', 'ぺ'),
		A2($author$project$Typing1$Rule, 'po', 'ぽ'),
		A2($author$project$Typing1$Rule, 'ma', 'ま'),
		A2($author$project$Typing1$Rule, 'mi', 'み'),
		A2($author$project$Typing1$Rule, 'mu', 'む'),
		A2($author$project$Typing1$Rule, 'me', 'め'),
		A2($author$project$Typing1$Rule, 'mo', 'も'),
		A2($author$project$Typing1$Rule, 'xya', 'ゃ'),
		A2($author$project$Typing1$Rule, 'lya', 'ゃ'),
		A2($author$project$Typing1$Rule, 'ya', 'や'),
		A2($author$project$Typing1$Rule, 'wyi', 'ゐ'),
		A2($author$project$Typing1$Rule, 'xyu', 'ゅ'),
		A2($author$project$Typing1$Rule, 'lyu', 'ゅ'),
		A2($author$project$Typing1$Rule, 'yu', 'ゆ'),
		A2($author$project$Typing1$Rule, 'wye', 'ゑ'),
		A2($author$project$Typing1$Rule, 'xyo', 'ょ'),
		A2($author$project$Typing1$Rule, 'lyo', 'ょ'),
		A2($author$project$Typing1$Rule, 'yo', 'よ'),
		A2($author$project$Typing1$Rule, 'ra', 'ら'),
		A2($author$project$Typing1$Rule, 'ri', 'り'),
		A2($author$project$Typing1$Rule, 'ru', 'る'),
		A2($author$project$Typing1$Rule, 're', 'れ'),
		A2($author$project$Typing1$Rule, 'ro', 'ろ'),
		A2($author$project$Typing1$Rule, 'xwa', 'ゎ'),
		A2($author$project$Typing1$Rule, 'lwa', 'ゎ'),
		A2($author$project$Typing1$Rule, 'wa', 'わ'),
		A2($author$project$Typing1$Rule, 'wi', 'うぃ'),
		A2($author$project$Typing1$Rule, 'we', 'うぇ'),
		A2($author$project$Typing1$Rule, 'wo', 'を'),
		A2($author$project$Typing1$Rule, 'wha', 'うぁ'),
		A2($author$project$Typing1$Rule, 'whi', 'うぃ'),
		A2($author$project$Typing1$Rule, 'whu', 'う'),
		A2($author$project$Typing1$Rule, 'whe', 'うぇ'),
		A2($author$project$Typing1$Rule, 'who', 'うぉ'),
		A2($author$project$Typing1$Rule, 'qqa', 'っくぁ'),
		A2($author$project$Typing1$Rule, 'qqi', 'っくぃ'),
		A2($author$project$Typing1$Rule, 'qqu', 'っく'),
		A2($author$project$Typing1$Rule, 'qqe', 'っくぇ'),
		A2($author$project$Typing1$Rule, 'qqo', 'っくぉ'),
		A2($author$project$Typing1$Rule, 'vva', 'っゔぁ'),
		A2($author$project$Typing1$Rule, 'vvi', 'っゔぃ'),
		A2($author$project$Typing1$Rule, 'vvu', 'っゔ'),
		A2($author$project$Typing1$Rule, 'vve', 'っゔぇ'),
		A2($author$project$Typing1$Rule, 'vvo', 'っゔぉ'),
		A2($author$project$Typing1$Rule, 'vvya', 'っゔゃ'),
		A2($author$project$Typing1$Rule, 'vvyi', 'っゔぃ'),
		A2($author$project$Typing1$Rule, 'vvyu', 'っゔゅ'),
		A2($author$project$Typing1$Rule, 'vvye', 'っゔぇ'),
		A2($author$project$Typing1$Rule, 'vvyo', 'っゔょ'),
		A2($author$project$Typing1$Rule, 'lla', 'っぁ'),
		A2($author$project$Typing1$Rule, 'lli', 'っぃ'),
		A2($author$project$Typing1$Rule, 'llu', 'っぅ'),
		A2($author$project$Typing1$Rule, 'lle', 'っぇ'),
		A2($author$project$Typing1$Rule, 'llo', 'っぉ'),
		A2($author$project$Typing1$Rule, 'llyi', 'っぃ'),
		A2($author$project$Typing1$Rule, 'llye', 'っぇ'),
		A2($author$project$Typing1$Rule, 'llka', 'っヵ'),
		A2($author$project$Typing1$Rule, 'llke', 'っヶ'),
		A2($author$project$Typing1$Rule, 'lltu', 'っっ'),
		A2($author$project$Typing1$Rule, 'llya', 'っゃ'),
		A2($author$project$Typing1$Rule, 'llyu', 'っゅ'),
		A2($author$project$Typing1$Rule, 'llyo', 'っょ'),
		A2($author$project$Typing1$Rule, 'llwa', 'っゎ'),
		A2($author$project$Typing1$Rule, 'xxn', 'っん'),
		A2($author$project$Typing1$Rule, 'xxa', 'っぁ'),
		A2($author$project$Typing1$Rule, 'xxi', 'っぃ'),
		A2($author$project$Typing1$Rule, 'xxu', 'っぅ'),
		A2($author$project$Typing1$Rule, 'xxe', 'っぇ'),
		A2($author$project$Typing1$Rule, 'xxo', 'っぉ'),
		A2($author$project$Typing1$Rule, 'xxyi', 'っぃ'),
		A2($author$project$Typing1$Rule, 'xxye', 'っぇ'),
		A2($author$project$Typing1$Rule, 'xxka', 'っヵ'),
		A2($author$project$Typing1$Rule, 'xxke', 'っヶ'),
		A2($author$project$Typing1$Rule, 'xxtu', 'っっ'),
		A2($author$project$Typing1$Rule, 'xxya', 'っゃ'),
		A2($author$project$Typing1$Rule, 'xxyu', 'っゅ'),
		A2($author$project$Typing1$Rule, 'xxyo', 'っょ'),
		A2($author$project$Typing1$Rule, 'xxwa', 'っゎ'),
		A2($author$project$Typing1$Rule, 'kkya', 'っきゃ'),
		A2($author$project$Typing1$Rule, 'kkyi', 'っきぃ'),
		A2($author$project$Typing1$Rule, 'kkyu', 'っきゅ'),
		A2($author$project$Typing1$Rule, 'kkye', 'っきぇ'),
		A2($author$project$Typing1$Rule, 'kkyo', 'っきょ'),
		A2($author$project$Typing1$Rule, 'kka', 'っか'),
		A2($author$project$Typing1$Rule, 'kki', 'っき'),
		A2($author$project$Typing1$Rule, 'kku', 'っく'),
		A2($author$project$Typing1$Rule, 'kke', 'っけ'),
		A2($author$project$Typing1$Rule, 'kko', 'っこ'),
		A2($author$project$Typing1$Rule, 'kkwa', 'っくぁ'),
		A2($author$project$Typing1$Rule, 'ggya', 'っぎゃ'),
		A2($author$project$Typing1$Rule, 'ggyi', 'っぎぃ'),
		A2($author$project$Typing1$Rule, 'ggyu', 'っぎゅ'),
		A2($author$project$Typing1$Rule, 'ggye', 'っぎぇ'),
		A2($author$project$Typing1$Rule, 'ggyo', 'っぎょ'),
		A2($author$project$Typing1$Rule, 'gga', 'っが'),
		A2($author$project$Typing1$Rule, 'ggi', 'っぎ'),
		A2($author$project$Typing1$Rule, 'ggu', 'っぐ'),
		A2($author$project$Typing1$Rule, 'gge', 'っげ'),
		A2($author$project$Typing1$Rule, 'ggo', 'っご'),
		A2($author$project$Typing1$Rule, 'ggwa', 'っぐぁ'),
		A2($author$project$Typing1$Rule, 'ggwi', 'っぐぃ'),
		A2($author$project$Typing1$Rule, 'ggwu', 'っぐぅ'),
		A2($author$project$Typing1$Rule, 'ggwe', 'っぐぇ'),
		A2($author$project$Typing1$Rule, 'ggwo', 'っぐぉ'),
		A2($author$project$Typing1$Rule, 'ssya', 'っしゃ'),
		A2($author$project$Typing1$Rule, 'ssyi', 'っしぃ'),
		A2($author$project$Typing1$Rule, 'ssyu', 'っしゅ'),
		A2($author$project$Typing1$Rule, 'ssye', 'っしぇ'),
		A2($author$project$Typing1$Rule, 'ssyo', 'っしょ'),
		A2($author$project$Typing1$Rule, 'ssha', 'っしゃ'),
		A2($author$project$Typing1$Rule, 'sshi', 'っし'),
		A2($author$project$Typing1$Rule, 'sshu', 'っしゅ'),
		A2($author$project$Typing1$Rule, 'sshe', 'っしぇ'),
		A2($author$project$Typing1$Rule, 'ssho', 'っしょ'),
		A2($author$project$Typing1$Rule, 'ssa', 'っさ'),
		A2($author$project$Typing1$Rule, 'ssi', 'っし'),
		A2($author$project$Typing1$Rule, 'ssu', 'っす'),
		A2($author$project$Typing1$Rule, 'sse', 'っせ'),
		A2($author$project$Typing1$Rule, 'sso', 'っそ'),
		A2($author$project$Typing1$Rule, 'zzya', 'っじゃ'),
		A2($author$project$Typing1$Rule, 'zzyi', 'っじぃ'),
		A2($author$project$Typing1$Rule, 'zzyu', 'っじゅ'),
		A2($author$project$Typing1$Rule, 'zzye', 'っじぇ'),
		A2($author$project$Typing1$Rule, 'zzyo', 'っじょ'),
		A2($author$project$Typing1$Rule, 'zza', 'っざ'),
		A2($author$project$Typing1$Rule, 'zzi', 'っじ'),
		A2($author$project$Typing1$Rule, 'zzu', 'っず'),
		A2($author$project$Typing1$Rule, 'zze', 'っぜ'),
		A2($author$project$Typing1$Rule, 'zzo', 'っぞ'),
		A2($author$project$Typing1$Rule, 'jja', 'っじゃ'),
		A2($author$project$Typing1$Rule, 'jji', 'っじ'),
		A2($author$project$Typing1$Rule, 'jju', 'っじゅ'),
		A2($author$project$Typing1$Rule, 'jje', 'っじぇ'),
		A2($author$project$Typing1$Rule, 'jjo', 'っじょ'),
		A2($author$project$Typing1$Rule, 'jjya', 'っじゃ'),
		A2($author$project$Typing1$Rule, 'jjyi', 'っじぃ'),
		A2($author$project$Typing1$Rule, 'jjyu', 'っじゅ'),
		A2($author$project$Typing1$Rule, 'jjye', 'っじぇ'),
		A2($author$project$Typing1$Rule, 'jjyo', 'っじょ'),
		A2($author$project$Typing1$Rule, 'ttya', 'っちゃ'),
		A2($author$project$Typing1$Rule, 'ttyi', 'っちぃ'),
		A2($author$project$Typing1$Rule, 'ttyu', 'っちゅ'),
		A2($author$project$Typing1$Rule, 'ttye', 'っちぇ'),
		A2($author$project$Typing1$Rule, 'ttyo', 'っちょ'),
		A2($author$project$Typing1$Rule, 'ttsa', 'っつぁ'),
		A2($author$project$Typing1$Rule, 'ttsi', 'っつぃ'),
		A2($author$project$Typing1$Rule, 'ttse', 'っつぇ'),
		A2($author$project$Typing1$Rule, 'ttso', 'っつぉ'),
		A2($author$project$Typing1$Rule, 'ttha', 'ってゃ'),
		A2($author$project$Typing1$Rule, 'tthi', 'ってぃ'),
		A2($author$project$Typing1$Rule, 'tthu', 'ってゅ'),
		A2($author$project$Typing1$Rule, 'tthe', 'ってぇ'),
		A2($author$project$Typing1$Rule, 'ttho', 'ってょ'),
		A2($author$project$Typing1$Rule, 'ttwa', 'っとぁ'),
		A2($author$project$Typing1$Rule, 'ttwi', 'っとぃ'),
		A2($author$project$Typing1$Rule, 'ttwu', 'っとぅ'),
		A2($author$project$Typing1$Rule, 'ttwe', 'っとぇ'),
		A2($author$project$Typing1$Rule, 'ttwo', 'っとぉ'),
		A2($author$project$Typing1$Rule, 'tta', 'った'),
		A2($author$project$Typing1$Rule, 'tti', 'っち'),
		A2($author$project$Typing1$Rule, 'ttu', 'っつ'),
		A2($author$project$Typing1$Rule, 'ttsu', 'っつ'),
		A2($author$project$Typing1$Rule, 'tte', 'って'),
		A2($author$project$Typing1$Rule, 'tto', 'っと'),
		A2($author$project$Typing1$Rule, 'ddya', 'っぢゃ'),
		A2($author$project$Typing1$Rule, 'ddyi', 'っぢぃ'),
		A2($author$project$Typing1$Rule, 'ddyu', 'っぢゅ'),
		A2($author$project$Typing1$Rule, 'ddye', 'っぢぇ'),
		A2($author$project$Typing1$Rule, 'ddyo', 'っぢょ'),
		A2($author$project$Typing1$Rule, 'ddha', 'っでゃ'),
		A2($author$project$Typing1$Rule, 'ddhi', 'っでぃ'),
		A2($author$project$Typing1$Rule, 'ddhu', 'っでゅ'),
		A2($author$project$Typing1$Rule, 'ddhe', 'っでぇ'),
		A2($author$project$Typing1$Rule, 'ddho', 'っでょ'),
		A2($author$project$Typing1$Rule, 'ddwa', 'っどぁ'),
		A2($author$project$Typing1$Rule, 'ddwi', 'っどぃ'),
		A2($author$project$Typing1$Rule, 'ddwu', 'っどぅ'),
		A2($author$project$Typing1$Rule, 'ddwe', 'っどぇ'),
		A2($author$project$Typing1$Rule, 'ddwo', 'っどぉ'),
		A2($author$project$Typing1$Rule, 'dda', 'っだ'),
		A2($author$project$Typing1$Rule, 'ddi', 'っぢ'),
		A2($author$project$Typing1$Rule, 'ddu', 'っづ'),
		A2($author$project$Typing1$Rule, 'dde', 'っで'),
		A2($author$project$Typing1$Rule, 'ddo', 'っど'),
		A2($author$project$Typing1$Rule, 'hhya', 'っひゃ'),
		A2($author$project$Typing1$Rule, 'hhyi', 'っひぃ'),
		A2($author$project$Typing1$Rule, 'hhyu', 'っひゅ'),
		A2($author$project$Typing1$Rule, 'hhye', 'っひぇ'),
		A2($author$project$Typing1$Rule, 'hhyo', 'っひょ'),
		A2($author$project$Typing1$Rule, 'hha', 'っは'),
		A2($author$project$Typing1$Rule, 'hhi', 'っひ'),
		A2($author$project$Typing1$Rule, 'hhu', 'っふ'),
		A2($author$project$Typing1$Rule, 'hhe', 'っへ'),
		A2($author$project$Typing1$Rule, 'hho', 'っほ'),
		A2($author$project$Typing1$Rule, 'ffa', 'っふぁ'),
		A2($author$project$Typing1$Rule, 'ffi', 'っふぃ'),
		A2($author$project$Typing1$Rule, 'ffu', 'っふ'),
		A2($author$project$Typing1$Rule, 'ffe', 'っふぇ'),
		A2($author$project$Typing1$Rule, 'ffo', 'っふぉ'),
		A2($author$project$Typing1$Rule, 'ffya', 'っふゃ'),
		A2($author$project$Typing1$Rule, 'ffyu', 'っふゅ'),
		A2($author$project$Typing1$Rule, 'ffyo', 'っふょ'),
		A2($author$project$Typing1$Rule, 'ffu', 'っふ'),
		A2($author$project$Typing1$Rule, 'bbya', 'っびゃ'),
		A2($author$project$Typing1$Rule, 'bbyi', 'っびぃ'),
		A2($author$project$Typing1$Rule, 'bbyu', 'っびゅ'),
		A2($author$project$Typing1$Rule, 'bbye', 'っびぇ'),
		A2($author$project$Typing1$Rule, 'bbyo', 'っびょ'),
		A2($author$project$Typing1$Rule, 'bba', 'っば'),
		A2($author$project$Typing1$Rule, 'bbi', 'っび'),
		A2($author$project$Typing1$Rule, 'bbu', 'っぶ'),
		A2($author$project$Typing1$Rule, 'bbe', 'っべ'),
		A2($author$project$Typing1$Rule, 'bbo', 'っぼ'),
		A2($author$project$Typing1$Rule, 'ppya', 'っぴゃ'),
		A2($author$project$Typing1$Rule, 'ppyi', 'っぴぃ'),
		A2($author$project$Typing1$Rule, 'ppyu', 'っぴゅ'),
		A2($author$project$Typing1$Rule, 'ppye', 'っぴぇ'),
		A2($author$project$Typing1$Rule, 'ppyo', 'っぴょ'),
		A2($author$project$Typing1$Rule, 'ppa', 'っぱ'),
		A2($author$project$Typing1$Rule, 'ppi', 'っぴ'),
		A2($author$project$Typing1$Rule, 'ppu', 'っぷ'),
		A2($author$project$Typing1$Rule, 'ppe', 'っぺ'),
		A2($author$project$Typing1$Rule, 'ppo', 'っぽ'),
		A2($author$project$Typing1$Rule, 'mmya', 'っみゃ'),
		A2($author$project$Typing1$Rule, 'mmyi', 'っみぃ'),
		A2($author$project$Typing1$Rule, 'mmyu', 'っみゅ'),
		A2($author$project$Typing1$Rule, 'mmye', 'っみぇ'),
		A2($author$project$Typing1$Rule, 'mmyo', 'っみょ'),
		A2($author$project$Typing1$Rule, 'mma', 'っま'),
		A2($author$project$Typing1$Rule, 'mmi', 'っみ'),
		A2($author$project$Typing1$Rule, 'mmu', 'っむ'),
		A2($author$project$Typing1$Rule, 'mme', 'っめ'),
		A2($author$project$Typing1$Rule, 'mmo', 'っも'),
		A2($author$project$Typing1$Rule, 'yye', 'っいぇ'),
		A2($author$project$Typing1$Rule, 'yya', 'っや'),
		A2($author$project$Typing1$Rule, 'yyu', 'っゆ'),
		A2($author$project$Typing1$Rule, 'yyo', 'っよ'),
		A2($author$project$Typing1$Rule, 'rrya', 'っりゃ'),
		A2($author$project$Typing1$Rule, 'rryi', 'っりぃ'),
		A2($author$project$Typing1$Rule, 'rryu', 'っりゅ'),
		A2($author$project$Typing1$Rule, 'rrye', 'っりぇ'),
		A2($author$project$Typing1$Rule, 'rryo', 'っりょ'),
		A2($author$project$Typing1$Rule, 'rra', 'っら'),
		A2($author$project$Typing1$Rule, 'rri', 'っり'),
		A2($author$project$Typing1$Rule, 'rru', 'っる'),
		A2($author$project$Typing1$Rule, 'rre', 'っれ'),
		A2($author$project$Typing1$Rule, 'rro', 'っろ'),
		A2($author$project$Typing1$Rule, 'wwu', 'っう'),
		A2($author$project$Typing1$Rule, 'wwyi', 'っゐ'),
		A2($author$project$Typing1$Rule, 'wwye', 'っゑ'),
		A2($author$project$Typing1$Rule, 'wwa', 'っわ'),
		A2($author$project$Typing1$Rule, 'wwi', 'っうぃ'),
		A2($author$project$Typing1$Rule, 'wwe', 'っうぇ'),
		A2($author$project$Typing1$Rule, 'wwo', 'っを'),
		A2($author$project$Typing1$Rule, 'wwha', 'っうぁ'),
		A2($author$project$Typing1$Rule, 'wwhi', 'っうぃ'),
		A2($author$project$Typing1$Rule, 'wwhu', 'っう'),
		A2($author$project$Typing1$Rule, 'wwhe', 'っうぇ'),
		A2($author$project$Typing1$Rule, 'wwho', 'っうぉ'),
		A2($author$project$Typing1$Rule, 'ccha', 'っちゃ'),
		A2($author$project$Typing1$Rule, 'cchi', 'っち'),
		A2($author$project$Typing1$Rule, 'cchu', 'っちゅ'),
		A2($author$project$Typing1$Rule, 'cche', 'っちぇ'),
		A2($author$project$Typing1$Rule, 'ccho', 'っちょ'),
		A2($author$project$Typing1$Rule, 'ccya', 'っちゃ'),
		A2($author$project$Typing1$Rule, 'ccyi', 'っちぃ'),
		A2($author$project$Typing1$Rule, 'ccyu', 'っちゅ'),
		A2($author$project$Typing1$Rule, 'ccye', 'っちぇ'),
		A2($author$project$Typing1$Rule, 'ccyo', 'っちょ'),
		A2($author$project$Typing1$Rule, 'cca', 'っか'),
		A2($author$project$Typing1$Rule, 'cci', 'っし'),
		A2($author$project$Typing1$Rule, 'ccu', 'っく'),
		A2($author$project$Typing1$Rule, 'cce', 'っせ'),
		A2($author$project$Typing1$Rule, 'cco', 'っこ'),
		A2($author$project$Typing1$Rule, 'a', 'a'),
		A2($author$project$Typing1$Rule, 'b', 'b'),
		A2($author$project$Typing1$Rule, 'c', 'c'),
		A2($author$project$Typing1$Rule, 'd', 'd'),
		A2($author$project$Typing1$Rule, 'e', 'e'),
		A2($author$project$Typing1$Rule, 'f', 'f'),
		A2($author$project$Typing1$Rule, 'g', 'g'),
		A2($author$project$Typing1$Rule, 'h', 'h'),
		A2($author$project$Typing1$Rule, 'i', 'i'),
		A2($author$project$Typing1$Rule, 'j', 'j'),
		A2($author$project$Typing1$Rule, 'k', 'k'),
		A2($author$project$Typing1$Rule, 'l', 'l'),
		A2($author$project$Typing1$Rule, 'm', 'm'),
		A2($author$project$Typing1$Rule, 'n', 'n'),
		A2($author$project$Typing1$Rule, 'o', 'o'),
		A2($author$project$Typing1$Rule, 'p', 'p'),
		A2($author$project$Typing1$Rule, 'q', 'q'),
		A2($author$project$Typing1$Rule, 'r', 'r'),
		A2($author$project$Typing1$Rule, 's', 's'),
		A2($author$project$Typing1$Rule, 't', 't'),
		A2($author$project$Typing1$Rule, 'u', 'u'),
		A2($author$project$Typing1$Rule, 'v', 'v'),
		A2($author$project$Typing1$Rule, 'w', 'w'),
		A2($author$project$Typing1$Rule, 'x', 'x'),
		A2($author$project$Typing1$Rule, 'y', 'y'),
		A2($author$project$Typing1$Rule, 'z', 'z'),
		A2($author$project$Typing1$Rule, 'A', 'A'),
		A2($author$project$Typing1$Rule, 'B', 'B'),
		A2($author$project$Typing1$Rule, 'C', 'C'),
		A2($author$project$Typing1$Rule, 'D', 'D'),
		A2($author$project$Typing1$Rule, 'E', 'E'),
		A2($author$project$Typing1$Rule, 'F', 'F'),
		A2($author$project$Typing1$Rule, 'G', 'G'),
		A2($author$project$Typing1$Rule, 'H', 'H'),
		A2($author$project$Typing1$Rule, 'I', 'I'),
		A2($author$project$Typing1$Rule, 'J', 'J'),
		A2($author$project$Typing1$Rule, 'K', 'K'),
		A2($author$project$Typing1$Rule, 'L', 'L'),
		A2($author$project$Typing1$Rule, 'M', 'M'),
		A2($author$project$Typing1$Rule, 'N', 'N'),
		A2($author$project$Typing1$Rule, 'O', 'O'),
		A2($author$project$Typing1$Rule, 'P', 'P'),
		A2($author$project$Typing1$Rule, 'Q', 'Q'),
		A2($author$project$Typing1$Rule, 'R', 'R'),
		A2($author$project$Typing1$Rule, 'S', 'S'),
		A2($author$project$Typing1$Rule, 'T', 'T'),
		A2($author$project$Typing1$Rule, 'U', 'U'),
		A2($author$project$Typing1$Rule, 'V', 'V'),
		A2($author$project$Typing1$Rule, 'W', 'W'),
		A2($author$project$Typing1$Rule, 'X', 'X'),
		A2($author$project$Typing1$Rule, 'Y', 'Y'),
		A2($author$project$Typing1$Rule, 'Z', 'Z'),
		A2($author$project$Typing1$Rule, '0', '0'),
		A2($author$project$Typing1$Rule, '1', '1'),
		A2($author$project$Typing1$Rule, '2', '2'),
		A2($author$project$Typing1$Rule, '3', '3'),
		A2($author$project$Typing1$Rule, '4', '4'),
		A2($author$project$Typing1$Rule, '5', '5'),
		A2($author$project$Typing1$Rule, '6', '6'),
		A2($author$project$Typing1$Rule, '7', '7'),
		A2($author$project$Typing1$Rule, '8', '8'),
		A2($author$project$Typing1$Rule, '9', '9'),
		A2($author$project$Typing1$Rule, '`', '`'),
		A2($author$project$Typing1$Rule, '~', '~'),
		A2($author$project$Typing1$Rule, '!', '!'),
		A2($author$project$Typing1$Rule, '@', '@'),
		A2($author$project$Typing1$Rule, '#', '#'),
		A2($author$project$Typing1$Rule, '$', '$'),
		A2($author$project$Typing1$Rule, '%', '%'),
		A2($author$project$Typing1$Rule, '^', '^'),
		A2($author$project$Typing1$Rule, '&', '&'),
		A2($author$project$Typing1$Rule, '*', '*'),
		A2($author$project$Typing1$Rule, '(', '('),
		A2($author$project$Typing1$Rule, ')', ')'),
		A2($author$project$Typing1$Rule, '-', '-'),
		A2($author$project$Typing1$Rule, '_', '_'),
		A2($author$project$Typing1$Rule, '=', '='),
		A2($author$project$Typing1$Rule, '+', '+'),
		A2($author$project$Typing1$Rule, '[', '['),
		A2($author$project$Typing1$Rule, ']', ']'),
		A2($author$project$Typing1$Rule, '{', '{'),
		A2($author$project$Typing1$Rule, '}', '}'),
		A2($author$project$Typing1$Rule, '\\', '\\'),
		A2($author$project$Typing1$Rule, '|', '|'),
		A2($author$project$Typing1$Rule, ';', ';'),
		A2($author$project$Typing1$Rule, ':', ':'),
		A2($author$project$Typing1$Rule, '\'', '\''),
		A2($author$project$Typing1$Rule, '\"', '\"'),
		A2($author$project$Typing1$Rule, ',', ','),
		A2($author$project$Typing1$Rule, '<', '<'),
		A2($author$project$Typing1$Rule, '.', '.'),
		A2($author$project$Typing1$Rule, '>', '>'),
		A2($author$project$Typing1$Rule, '/', '/'),
		A2($author$project$Typing1$Rule, '?', '?'),
		A2($author$project$Typing1$Rule, ' ', ' ')
	]);
var $author$project$Typing1$newData = function (words) {
	return A3(
		$author$project$Typing1$Data,
		'',
		words,
		A3($author$project$Typing1$State, '', $elm$core$Maybe$Nothing, $author$project$Typing1$romanTable));
};
var $author$project$Page$TypeLongWord$initInTrial = F2(
	function (wordForView, wordForInput) {
		var typingData = $elm$core$Maybe$Just(
			$author$project$Typing1$newData(wordForInput));
		var lw = {id: -1, title: 'Trial', wordForInput: wordForInput, wordForView: wordForView};
		return _Utils_Tuple2(
			$author$project$Page$TypeLongWord$Model(
				$author$project$Page$TypeLongWord$Trial(lw))($author$project$Page$TypeLongWord$Waiting)('')(typingData)(wordForView)(0)(false)(
				$elm$time$Time$millisToPosix(0))(
				$elm$time$Time$millisToPosix(0))($elm$core$Maybe$Nothing)(_List_Nil)('')($elm$core$Maybe$Nothing),
			$elm$core$Platform$Cmd$none);
	});
var $author$project$Data$LongWord$trim = function (lw) {
	return {
		id: lw.id,
		title: $elm$core$String$trim(lw.title),
		wordForInput: $elm$core$String$trim(lw.wordForInput),
		wordForView: $elm$core$String$trim(lw.wordForView)
	};
};
var $author$project$Data$LongWord$encodeLongWord = function (rLongWord) {
	var longWord = $author$project$Data$LongWord$trim(rLongWord);
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(longWord.title)),
				_Utils_Tuple2(
				'wordForView',
				$elm$json$Json$Encode$string(longWord.wordForView)),
				_Utils_Tuple2(
				'wordForInput',
				$elm$json$Json$Encode$string(longWord.wordForInput))
			]));
};
var $author$project$API$encodeForPostLongWord = F2(
	function (user, longWord) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'user',
					$author$project$Data$User$encodeUser(user)),
					_Utils_Tuple2(
					'longWord',
					$author$project$Data$LongWord$encodeLongWord(longWord))
				]));
	});
var $elm$http$Http$expectString = function (toMsg) {
	return A2(
		$elm$http$Http$expectStringResponse,
		toMsg,
		$elm$http$Http$resolve($elm$core$Result$Ok));
};
var $author$project$API$postLongWord = F3(
	function (msg, user, lw) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					A2($author$project$API$encodeForPostLongWord, user, lw)),
				expect: $elm$http$Http$expectString(msg),
				url: $author$project$API$host + ($author$project$API$api + '?postLongWord')
			});
	});
var $author$project$Page$TypeLongWord$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Page$TypeLongWord$Finish = {$: 'Finish'};
var $author$project$Page$TypeLongWord$FinishTime = function (a) {
	return {$: 'FinishTime', a: a};
};
var $author$project$Page$TypeLongWord$GetRanking = {$: 'GetRanking'};
var $author$project$Page$TypeLongWord$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$Page$TypeLongWord$ReceiveDeleteWord = function (a) {
	return {$: 'ReceiveDeleteWord', a: a};
};
var $author$project$Page$TypeLongWord$ReceiveGetRanking = function (a) {
	return {$: 'ReceiveGetRanking', a: a};
};
var $author$project$Page$TypeLongWord$ReceiveRegistRanking = function (a) {
	return {$: 'ReceiveRegistRanking', a: a};
};
var $author$project$Page$TypeLongWord$StartTime = function (a) {
	return {$: 'StartTime', a: a};
};
var $author$project$Page$TypeLongWord$Typing = {$: 'Typing'};
var $author$project$API$encodeForDeleteLongWord = F2(
	function (user, wid) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'wordID',
					$elm$json$Json$Encode$int(wid)),
					_Utils_Tuple2(
					'user',
					$author$project$Data$User$encodeUser(user))
				]));
	});
var $author$project$API$deleteLongWord = F3(
	function (msg, user, wid) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					A2($author$project$API$encodeForDeleteLongWord, user, wid)),
				expect: $elm$http$Http$expectString(msg),
				url: $author$project$API$host + ($author$project$API$api + '?deleteLongWord')
			});
	});
var $author$project$API$encodeForGetLongWordRanking = F2(
	function (user, wid) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'user',
					$author$project$Data$User$encodeUser(user)),
					_Utils_Tuple2(
					'wordID',
					$elm$json$Json$Encode$int(wid))
				]));
	});
var $author$project$API$LongWordRankingScore = F4(
	function (rank, score, name, your) {
		return {name: name, rank: rank, score: score, your: your};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Data$LongWord$Score = F3(
	function (time, keys, miss) {
		return {keys: keys, miss: miss, time: time};
	});
var $author$project$Data$LongWord$scoreDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Data$LongWord$Score,
	A2($elm$json$Json$Decode$field, 'time', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'keys', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'miss', $elm$json$Json$Decode$int));
var $author$project$API$longWordRankingScoreDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$API$LongWordRankingScore,
	A2($elm$json$Json$Decode$field, 'rank', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'score', $author$project$Data$LongWord$scoreDecoder),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'your', $elm$json$Json$Decode$bool));
var $author$project$API$longWordRankingDecoder = $elm$json$Json$Decode$list($author$project$API$longWordRankingScoreDecoder);
var $author$project$API$getLongWordRanking = F3(
	function (msg, user, wid) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					A2($author$project$API$encodeForGetLongWordRanking, user, wid)),
				expect: A2($elm$http$Http$expectJson, msg, $author$project$API$longWordRankingDecoder),
				url: $author$project$API$host + ($author$project$API$api + '?getLongWordRanking')
			});
	});
var $author$project$Data$LongWord$encodeScore = function (v) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'time',
				$elm$json$Json$Encode$int(v.time)),
				_Utils_Tuple2(
				'keys',
				$elm$json$Json$Encode$int(v.keys)),
				_Utils_Tuple2(
				'miss',
				$elm$json$Json$Encode$int(v.miss))
			]));
};
var $author$project$API$encodeForRegistLongWordRanking = F3(
	function (user, wid, score) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'user',
					$author$project$Data$User$encodeUser(user)),
					_Utils_Tuple2(
					'wordID',
					$elm$json$Json$Encode$int(wid)),
					_Utils_Tuple2(
					'score',
					$author$project$Data$LongWord$encodeScore(score))
				]));
	});
var $author$project$API$registLongWordRanking = F4(
	function (msg, user, wid, score) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					A3($author$project$API$encodeForRegistLongWordRanking, user, wid, score)),
				expect: $elm$http$Http$expectString(msg),
				url: $author$project$API$host + ($author$project$API$api + '?registLongWordRanking')
			});
	});
var $author$project$Page$TypeLongWord$reset = F2(
	function (model, env) {
		var _v0 = $author$project$Page$TypeLongWord$init(env);
		var nm = _v0.a;
		var cmd = _v0.b;
		return _Utils_Tuple3(
			_Utils_update(
				nm,
				{
					bestScore: model.bestScore,
					modelState: model.modelState,
					ranking: model.ranking,
					scoreHistory: model.scoreHistory,
					typingData: function () {
						var _v1 = model.modelState;
						switch (_v1.$) {
							case 'Init':
								return $elm$core$Maybe$Nothing;
							case 'Error':
								return $elm$core$Maybe$Nothing;
							case 'Loaded':
								var lw = _v1.a;
								return $elm$core$Maybe$Just(
									$author$project$Typing1$newData(lw.wordForInput));
							default:
								var lw = _v1.a;
								return $elm$core$Maybe$Just(
									$author$project$Typing1$newData(lw.wordForInput));
						}
					}(),
					wordForView: function () {
						var _v2 = model.modelState;
						switch (_v2.$) {
							case 'Init':
								return '';
							case 'Error':
								return '';
							case 'Loaded':
								var lw = _v2.a;
								return lw.wordForView;
							default:
								var lw = _v2.a;
								return lw.wordForView;
						}
					}()
				}),
			$elm$core$Platform$Cmd$none,
			env);
	});
var $author$project$Page$TypeLongWord$run = function (m) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always(m),
		$elm$core$Task$succeed(_Utils_Tuple0));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$Typing1$nextData = F2(
	function (fixedRule, data) {
		return A3(
			$author$project$Typing1$Data,
			_Utils_ap(data.fixedWords, fixedRule.output),
			A2(
				$elm$core$String$dropLeft,
				$elm$core$String$length(fixedRule.output),
				data.restWords),
			A3($author$project$Typing1$State, '', $elm$core$Maybe$Nothing, $author$project$Typing1$romanTable));
	});
var $author$project$Typing1$typeTo = F2(
	function (input, data) {
		typeTo:
		while (true) {
			var sumInput = _Utils_ap(data.state.inputBuffer, input);
			var nextCandidates = A2(
				$elm$core$List$filter,
				function (r) {
					return A2($elm$core$String$startsWith, sumInput, r.input);
				},
				data.state.candidates);
			var nl = $elm$core$List$length(nextCandidates);
			var acceptCandidates = A2(
				$elm$core$List$filter,
				function (r) {
					return A2($elm$core$String$startsWith, r.output, data.restWords);
				},
				nextCandidates);
			var al = $elm$core$List$length(acceptCandidates);
			var tmpFixed = $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (r) {
						return _Utils_eq(sumInput, r.input);
					},
					acceptCandidates));
			if ((nl > 0) && (!al)) {
				return $elm$core$Maybe$Nothing;
			} else {
				if ((!nl) && (!al)) {
					var _v0 = data.state.tmpFixed;
					if (_v0.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var tmp = _v0.a;
						var $temp$input = input,
							$temp$data = A2($author$project$Typing1$nextData, tmp, data);
						input = $temp$input;
						data = $temp$data;
						continue typeTo;
					}
				} else {
					if (al === 1) {
						if (tmpFixed.$ === 'Nothing') {
							return $elm$core$Maybe$Just(
								A3(
									$author$project$Typing1$Data,
									data.fixedWords,
									data.restWords,
									A3($author$project$Typing1$State, sumInput, tmpFixed, nextCandidates)));
						} else {
							var tmp = tmpFixed.a;
							return $elm$core$Maybe$Just(
								A2($author$project$Typing1$nextData, tmp, data));
						}
					} else {
						return $elm$core$Maybe$Just(
							A3(
								$author$project$Typing1$Data,
								data.fixedWords,
								data.restWords,
								A3($author$project$Typing1$State, sumInput, tmpFixed, nextCandidates)));
					}
				}
			}
		}
	});
var $author$project$Page$TypeLongWord$update = F3(
	function (msg, model, env) {
		switch (msg.$) {
			case 'ReceiveLongWord':
				if (msg.a.$ === 'Ok') {
					var lw = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								modelState: $author$project$Page$TypeLongWord$Loaded(lw),
								typingData: $elm$core$Maybe$Just(
									$author$project$Typing1$newData(lw.wordForInput)),
								wordForView: lw.wordForView
							}),
						$author$project$Page$TypeLongWord$run($author$project$Page$TypeLongWord$GetRanking),
						env);
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								modelState: $author$project$Page$TypeLongWord$Error('お題取得失敗')
							}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'KeyDown':
				var key = msg.a;
				var _v1 = _Utils_Tuple2(model.modelState, model.typingData);
				_v1$4:
				while (true) {
					switch (_v1.a.$) {
						case 'Init':
							var _v2 = _v1.a;
							return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
						case 'Error':
							return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
						case 'Loaded':
							if (_v1.b.$ === 'Nothing') {
								var _v3 = _v1.b;
								return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
							} else {
								break _v1$4;
							}
						default:
							if (_v1.b.$ === 'Nothing') {
								var _v4 = _v1.b;
								return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
							} else {
								break _v1$4;
							}
					}
				}
				var typingData = _v1.b.a;
				switch (key) {
					case 'Shift':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Enter':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Backspace':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Control':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Tab':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Escape':
						return A2($author$project$Page$TypeLongWord$reset, model, env);
					default:
						var _v6 = model.typingState;
						switch (_v6.$) {
							case 'Waiting':
								var stc = A2($elm$core$Task$perform, $author$project$Page$TypeLongWord$StartTime, $elm$time$Time$now);
								var _v7 = A2($author$project$Typing1$typeTo, key, typingData);
								if (_v7.$ === 'Nothing') {
									return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
								} else {
									var d = _v7.a;
									var st = (d.restWords === '') ? $author$project$Page$TypeLongWord$Finish : $author$project$Page$TypeLongWord$Typing;
									var ftc = _Utils_eq(st, $author$project$Page$TypeLongWord$Finish) ? A2($elm$core$Task$perform, $author$project$Page$TypeLongWord$FinishTime, $elm$time$Time$now) : $elm$core$Platform$Cmd$none;
									return _Utils_Tuple3(
										_Utils_update(
											model,
											{
												inputHistory: _Utils_ap(model.inputHistory, key),
												missed: false,
												typingData: $elm$core$Maybe$Just(d),
												typingState: st
											}),
										$elm$core$Platform$Cmd$batch(
											_List_fromArray(
												[ftc, stc])),
										env);
								}
							case 'Typing':
								var _v8 = A2($author$project$Typing1$typeTo, key, typingData);
								if (_v8.$ === 'Nothing') {
									return _Utils_Tuple3(
										_Utils_update(
											model,
											{miss: model.miss + 1, missed: true}),
										$elm$core$Platform$Cmd$none,
										env);
								} else {
									var d = _v8.a;
									var st = (d.restWords === '') ? $author$project$Page$TypeLongWord$Finish : $author$project$Page$TypeLongWord$Typing;
									var ftc = _Utils_eq(st, $author$project$Page$TypeLongWord$Finish) ? A2($elm$core$Task$perform, $author$project$Page$TypeLongWord$FinishTime, $elm$time$Time$now) : $elm$core$Platform$Cmd$none;
									return _Utils_Tuple3(
										_Utils_update(
											model,
											{
												inputHistory: _Utils_ap(model.inputHistory, key),
												missed: false,
												typingData: $elm$core$Maybe$Just(d),
												typingState: st
											}),
										ftc,
										env);
								}
							default:
								return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
						}
				}
			case 'StartTime':
				var time = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{startTime: time}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'FinishTime':
				var time = msg.a;
				var score = {
					keys: $elm$core$String$length(model.inputHistory),
					miss: model.miss,
					time: $elm$time$Time$posixToMillis(time) - $elm$time$Time$posixToMillis(model.startTime)
				};
				var su = function () {
					var _v9 = model.bestScore;
					if (_v9.$ === 'Just') {
						var bs = _v9.a;
						return (_Utils_cmp(bs.time, score.time) > 0) ? true : false;
					} else {
						return true;
					}
				}();
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							bestScore: su ? $elm$core$Maybe$Just(score) : model.bestScore,
							finishTime: time,
							scoreHistory: A2($elm$core$List$cons, score, model.scoreHistory)
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'DeleteWord':
				var _v10 = model.modelState;
				if (_v10.$ === 'Loaded') {
					var lw = _v10.a;
					return _Utils_Tuple3(
						model,
						A3($author$project$API$deleteLongWord, $author$project$Page$TypeLongWord$ReceiveDeleteWord, env.user, lw.id),
						env);
				} else {
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				}
			case 'ReceiveDeleteWord':
				if (msg.a.$ === 'Ok') {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: str}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'エラー'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'RegistRanking':
				var _v11 = _Utils_Tuple2(model.modelState, model.bestScore);
				if (_v11.b.$ === 'Just') {
					if (_v11.a.$ === 'Loaded') {
						var lw = _v11.a.a;
						var score = _v11.b.a;
						return _Utils_Tuple3(
							model,
							A4($author$project$API$registLongWordRanking, $author$project$Page$TypeLongWord$ReceiveRegistRanking, env.user, lw.id, score),
							env);
					} else {
						return _Utils_Tuple3(
							_Utils_update(
								model,
								{notice: 'エラー'}),
							$elm$core$Platform$Cmd$none,
							env);
					}
				} else {
					var _v12 = _v11.b;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: '送るスコアがありません'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'ReceiveRegistRanking':
				if (msg.a.$ === 'Ok') {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: str}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'エラー'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'GetRanking':
				var _v13 = model.modelState;
				if (_v13.$ === 'Loaded') {
					var lw = _v13.a;
					return _Utils_Tuple3(
						model,
						A3($author$project$API$getLongWordRanking, $author$project$Page$TypeLongWord$ReceiveGetRanking, env.user, lw.id),
						env);
				} else {
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				}
			default:
				if (msg.a.$ === 'Ok') {
					var lwr = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								ranking: $elm$core$Maybe$Just(lwr)
							}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'ランキング取得失敗'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
		}
	});
var $author$project$Page$PostLongWord$update = F3(
	function (msg, model, env) {
		var pLW = model.lw;
		switch (msg.$) {
			case 'InputTitle':
				var newInput = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							lw: _Utils_update(
								pLW,
								{title: newInput})
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'InputWordForInput':
				var newInput = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							lw: _Utils_update(
								pLW,
								{wordForInput: newInput})
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'InputWordForView':
				var newInput = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							lw: _Utils_update(
								pLW,
								{wordForView: newInput})
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'Post':
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$PostLongWord$Edit(
								$author$project$Page$PostLongWord$Waiting('登録中'))
						}),
					A3($author$project$API$postLongWord, $author$project$Page$PostLongWord$ReceivePost, env.user, model.lw),
					env);
			case 'ReceivePost':
				if (msg.a.$ === 'Ok') {
					var res = msg.a.a;
					var st = function () {
						if (res === '登録成功') {
							return $author$project$Page$PostLongWord$Edit(
								$author$project$Page$PostLongWord$Success(res));
						} else {
							return $author$project$Page$PostLongWord$Edit(
								$author$project$Page$PostLongWord$Error('登録失敗: ' + res));
						}
					}();
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{state: st}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				}
			case 'TryType':
				var _v2 = A2($author$project$Page$TypeLongWord$initInTrial, model.lw.wordForView, model.lw.wordForInput);
				var pageModel = _v2.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$PostLongWord$Trial(pageModel)
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'BackEdit':
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$PostLongWord$Edit($author$project$Page$PostLongWord$Init)
						}),
					$elm$core$Platform$Cmd$none,
					env);
			default:
				var subMsg = msg.a;
				var _v3 = model.state;
				if (_v3.$ === 'Trial') {
					var subModel = _v3.a;
					var _v4 = A3($author$project$Page$TypeLongWord$update, subMsg, subModel, env);
					var newModel = _v4.a;
					var topCmd = _v4.b;
					var newEnv = _v4.c;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$PostLongWord$Trial(newModel)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Page$PostLongWord$TypeLongWordMsg, topCmd),
						newEnv);
				} else {
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				}
		}
	});
var $author$project$Page$Top$Init = {$: 'Init'};
var $author$project$Page$Top$update = F3(
	function (msg, model, env) {
		return _Utils_Tuple3(
			_Utils_update(
				model,
				{state: $author$project$Page$Top$Init}),
			$elm$core$Platform$Cmd$none,
			env);
	});
var $author$project$Page$TypeAny$reset = function (model) {
	return A4(
		$author$project$Page$TypeAny$Model,
		'',
		0,
		0,
		$elm$time$Time$millisToPosix(0));
};
var $author$project$Page$TypeAny$updateKPM = F2(
	function (time, model) {
		var st = $elm$time$Time$posixToMillis(model.lastTime);
		var lt = $elm$time$Time$posixToMillis(time);
		var keys = $elm$core$String$length(model.input);
		var interval = lt - st;
		var kpm = keys / (interval / (1000 * 60));
		var newBestKPM = A2($elm$core$Basics$max, kpm, model.bestKPM);
		return _Utils_update(
			model,
			{bestKPM: newBestKPM, lastTime: time, nowKPM: kpm});
	});
var $author$project$Page$TypeAny$update = F3(
	function (msg, model, env) {
		switch (msg.$) {
			case 'KeyDown':
				var key = msg.a;
				if (key === 'Escape') {
					return _Utils_Tuple3(
						$author$project$Page$TypeAny$reset(model),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								input: _Utils_ap(
									model.input,
									A2($elm$core$String$left, 1, key))
							}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'OneSecond':
				var time = msg.a;
				var newModel = A2($author$project$Page$TypeAny$updateKPM, time, model);
				return _Utils_Tuple3(
					_Utils_update(
						newModel,
						{input: ''}),
					$elm$core$Platform$Cmd$none,
					env);
			default:
				var time = msg.a;
				return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
		}
	});
var $author$project$Page$TypeShortWord$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Page$TypeShortWord$FinishTime = function (a) {
	return {$: 'FinishTime', a: a};
};
var $author$project$Page$TypeShortWord$StartTime = function (a) {
	return {$: 'StartTime', a: a};
};
var $GoldentTuft$elm_japanese_typing$Typing$getState = function (_v0) {
	var data = _v0.a;
	return data.state;
};
var $author$project$Page$TypeShortWord$maxWords = function (list) {
	return A2(
		$elm$core$Basics$min,
		$elm$core$List$length(list),
		10);
};
var $author$project$Page$TypeShortWord$reset = function (model) {
	var _v0 = model.state;
	switch (_v0.$) {
		case 'Init':
			return model;
		case 'Error':
			return model;
		default:
			var readyData = _v0.a;
			return _Utils_update(
				model,
				{
					state: $author$project$Page$TypeShortWord$Ready(
						_Utils_update(
							readyData,
							{
								countDownTimer: $author$project$Page$TypeShortWord$initialCountDownTimer,
								customTypingWords: $author$project$Page$TypeShortWord$initCustomTypingWords(readyData.shortWords.words)
							}))
				});
	}
};
var $author$project$Page$TypeShortWord$setCountDownTimer = F2(
	function (timer, model) {
		var _v0 = model.state;
		switch (_v0.$) {
			case 'Init':
				return model;
			case 'Error':
				return model;
			default:
				var readyData = _v0.a;
				return _Utils_update(
					model,
					{
						state: $author$project$Page$TypeShortWord$Ready(
							_Utils_update(
								readyData,
								{countDownTimer: timer}))
					});
		}
	});
var $author$project$Page$TypeShortWord$updateCurrentWord = F2(
	function (newWord, words) {
		var newRest = function () {
			var _v0 = words.rest;
			if (_v0.b) {
				var a = _v0.a;
				var b = _v0.b;
				return A2($elm$core$List$cons, newWord, b);
			} else {
				return _List_Nil;
			}
		}();
		return _Utils_update(
			words,
			{rest: newRest});
	});
var $author$project$Page$TypeShortWord$setRhythmTimer = F2(
	function (timer, model) {
		var _v0 = model.state;
		switch (_v0.$) {
			case 'Init':
				return model;
			case 'Error':
				return model;
			default:
				var readyData = _v0.a;
				var _v1 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
				if (_v1.$ === 'Nothing') {
					return model;
				} else {
					var word = _v1.a;
					var newWord = _Utils_update(
						word,
						{rhythmTimer: timer});
					var newWords = A2($author$project$Page$TypeShortWord$updateCurrentWord, newWord, readyData.customTypingWords);
					return _Utils_update(
						model,
						{
							state: $author$project$Page$TypeShortWord$Ready(
								_Utils_update(
									readyData,
									{customTypingWords: newWords}))
						});
				}
		}
	});
var $author$project$CountDown$setStart = function (_v0) {
	var timer = _v0.a;
	return $author$project$CountDown$Timer(
		_Utils_update(
			timer,
			{start: true}));
};
var $author$project$Page$TypeShortWord$startCountDownTimer = function (model) {
	var _v0 = $author$project$Page$TypeShortWord$getCountDownTimer(model);
	if (_v0.$ === 'Nothing') {
		return model;
	} else {
		var timer = _v0.a;
		return A2(
			$author$project$Page$TypeShortWord$setCountDownTimer,
			$author$project$CountDown$setStart(timer),
			model);
	}
};
var $GoldentTuft$elm_japanese_typing$Typing$Miss = {$: 'Miss'};
var $GoldentTuft$elm_japanese_typing$Typing$Typing = {$: 'Typing'};
var $GoldentTuft$elm_japanese_typing$Typing$Finish = {$: 'Finish'};
var $GoldentTuft$elm_japanese_typing$Typing$nextData = F2(
	function (fixedRule, _v0) {
		var data = _v0.a;
		var newRest = A2(
			$elm$core$String$dropLeft,
			$elm$core$String$length(fixedRule.output),
			data.restWords);
		var newFix = _Utils_ap(data.fixedWords, fixedRule.output);
		return $GoldentTuft$elm_japanese_typing$Typing$Data(
			{
				convertBuf: A3($GoldentTuft$elm_japanese_typing$Typing$ConvertBuf, '', $elm$core$Maybe$Nothing, data.rules),
				fixedWords: newFix,
				history: data.history,
				restWords: newRest,
				rules: data.rules,
				state: (!$elm$core$String$length(newRest)) ? $GoldentTuft$elm_japanese_typing$Typing$Finish : $GoldentTuft$elm_japanese_typing$Typing$Typing
			});
	});
var $GoldentTuft$elm_japanese_typing$Typing$typeTo_ = F2(
	function (input, _v0) {
		typeTo_:
		while (true) {
			var data = _v0.a;
			var sumInput = _Utils_ap(data.convertBuf.inputBuffer, input);
			var nextCandidates = A2(
				$elm$core$List$filter,
				function (r) {
					return A2($elm$core$String$startsWith, sumInput, r.input);
				},
				data.convertBuf.candidates);
			var nl = $elm$core$List$length(nextCandidates);
			var acceptCandidates = A2(
				$elm$core$List$filter,
				function (r) {
					return A2($elm$core$String$startsWith, r.output, data.restWords);
				},
				nextCandidates);
			var al = $elm$core$List$length(acceptCandidates);
			var tmpFixed = $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (r) {
						return _Utils_eq(sumInput, r.input);
					},
					acceptCandidates));
			if ((nl > 0) && (!al)) {
				return $GoldentTuft$elm_japanese_typing$Typing$Data(
					_Utils_update(
						data,
						{state: $GoldentTuft$elm_japanese_typing$Typing$Miss}));
			} else {
				if ((!nl) && (!al)) {
					var _v1 = data.convertBuf.tmpFixed;
					if (_v1.$ === 'Nothing') {
						return $GoldentTuft$elm_japanese_typing$Typing$Data(
							_Utils_update(
								data,
								{state: $GoldentTuft$elm_japanese_typing$Typing$Miss}));
					} else {
						var tmp = _v1.a;
						var $temp$input = input,
							$temp$_v0 = A2(
							$GoldentTuft$elm_japanese_typing$Typing$nextData,
							tmp,
							$GoldentTuft$elm_japanese_typing$Typing$Data(data));
						input = $temp$input;
						_v0 = $temp$_v0;
						continue typeTo_;
					}
				} else {
					if (al === 1) {
						if (tmpFixed.$ === 'Nothing') {
							return $GoldentTuft$elm_japanese_typing$Typing$Data(
								_Utils_update(
									data,
									{
										convertBuf: A3($GoldentTuft$elm_japanese_typing$Typing$ConvertBuf, sumInput, tmpFixed, nextCandidates),
										state: $GoldentTuft$elm_japanese_typing$Typing$Typing
									}));
						} else {
							var tmp = tmpFixed.a;
							return A2(
								$GoldentTuft$elm_japanese_typing$Typing$nextData,
								tmp,
								$GoldentTuft$elm_japanese_typing$Typing$Data(data));
						}
					} else {
						return $GoldentTuft$elm_japanese_typing$Typing$Data(
							_Utils_update(
								data,
								{
									convertBuf: A3($GoldentTuft$elm_japanese_typing$Typing$ConvertBuf, sumInput, tmpFixed, nextCandidates),
									state: $GoldentTuft$elm_japanese_typing$Typing$Typing
								}));
					}
				}
			}
		}
	});
var $GoldentTuft$elm_japanese_typing$Typing$typeTo = F2(
	function (input, _v0) {
		var data = _v0.a;
		return A2(
			$GoldentTuft$elm_japanese_typing$Typing$typeTo_,
			input,
			$GoldentTuft$elm_japanese_typing$Typing$Data(
				_Utils_update(
					data,
					{
						history: _Utils_ap(data.history, input)
					})));
	});
var $author$project$Page$TypeShortWord$updateByCorrect = F4(
	function (key, newTypingData, word, words) {
		var newWords = _Utils_update(
			words,
			{missed: false});
		var newWord = _Utils_update(
			word,
			{typingData: newTypingData});
		return A2($author$project$Page$TypeShortWord$updateCurrentWord, newWord, newWords);
	});
var $author$project$Page$TypeShortWord$updateByMiss = F2(
	function (word, words) {
		var newWords = _Utils_update(
			words,
			{miss: words.miss + 1, missed: true});
		var newWord = _Utils_update(
			word,
			{miss: word.miss + 1});
		return A2($author$project$Page$TypeShortWord$updateCurrentWord, newWord, newWords);
	});
var $author$project$Page$TypeShortWord$update = F3(
	function (msg, model, env) {
		switch (msg.$) {
			case 'ReceiveShortWords':
				if (msg.a.$ === 'Ok') {
					var sw = msg.a.a;
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$TypeShortWord$Error('お題取得失敗')
							}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'ShuffleWords':
				var list = msg.a;
				var _v1 = model.state;
				switch (_v1.$) {
					case 'Init':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Error':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					default:
						var readyData = _v1.a;
						return _Utils_Tuple3(
							_Utils_update(
								model,
								{
									state: $author$project$Page$TypeShortWord$Ready(
										_Utils_update(
											readyData,
											{
												customTypingWords: $author$project$Page$TypeShortWord$initCustomTypingWords(
													A2(
														$elm$core$List$take,
														$author$project$Page$TypeShortWord$maxWords(list),
														list))
											}))
								}),
							$elm$core$Platform$Cmd$none,
							env);
				}
			case 'KeyDown':
				var key = msg.a;
				var _v2 = model.state;
				switch (_v2.$) {
					case 'Init':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Error':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					default:
						var readyData = _v2.a;
						var _v3 = _Utils_Tuple2(
							_Utils_eq(
								$author$project$CountDown$getState(readyData.countDownTimer),
								$author$project$CountDown$Zero),
							key);
						_v3$0:
						while (true) {
							if (!_v3.a) {
								switch (_v3.b) {
									case 'Escape':
										break _v3$0;
									case ' ':
										return _Utils_Tuple3(
											$author$project$Page$TypeShortWord$startCountDownTimer(model),
											$elm$core$Platform$Cmd$none,
											env);
									default:
										return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
								}
							} else {
								if (_v3.b === 'Escape') {
									break _v3$0;
								} else {
									var _v4 = function () {
										var _v5 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
										if (_v5.$ === 'Nothing') {
											return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
										} else {
											var customTypingWord = _v5.a;
											var _v6 = _Utils_eq(
												$author$project$CountDown$getState(customTypingWord.rhythmTimer),
												$author$project$CountDown$Zero);
											if (!_v6) {
												return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
											} else {
												var nowT = customTypingWord.typingData;
												var nowS = $GoldentTuft$elm_japanese_typing$Typing$getState(nowT);
												var newT = A2($GoldentTuft$elm_japanese_typing$Typing$typeTo, key, nowT);
												var newS = $GoldentTuft$elm_japanese_typing$Typing$getState(newT);
												switch (nowS.$) {
													case 'Waiting':
														switch (newS.$) {
															case 'Waiting':
																return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
															case 'Typing':
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															case 'Miss':
																return _Utils_Tuple2(
																	A2($author$project$Page$TypeShortWord$updateByMiss, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															default:
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
														}
													case 'Typing':
														switch (newS.$) {
															case 'Waiting':
																return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
															case 'Typing':
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															case 'Miss':
																return _Utils_Tuple2(
																	A2($author$project$Page$TypeShortWord$updateByMiss, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															default:
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	A2($elm$core$Task$perform, $author$project$Page$TypeShortWord$FinishTime, $elm$time$Time$now));
														}
													case 'Miss':
														switch (newS.$) {
															case 'Waiting':
																return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
															case 'Typing':
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															case 'Miss':
																return _Utils_Tuple2(
																	A2($author$project$Page$TypeShortWord$updateByMiss, customTypingWord, readyData.customTypingWords),
																	$elm$core$Platform$Cmd$none);
															default:
																return _Utils_Tuple2(
																	A4($author$project$Page$TypeShortWord$updateByCorrect, key, newT, customTypingWord, readyData.customTypingWords),
																	A2($elm$core$Task$perform, $author$project$Page$TypeShortWord$FinishTime, $elm$time$Time$now));
														}
													default:
														return _Utils_Tuple2(readyData.customTypingWords, $elm$core$Platform$Cmd$none);
												}
											}
										}
									}();
									var newCustomTypingWords = _v4.a;
									var resCmd = _v4.b;
									return _Utils_Tuple3(
										_Utils_update(
											model,
											{
												state: $author$project$Page$TypeShortWord$Ready(
													_Utils_update(
														readyData,
														{customTypingWords: newCustomTypingWords}))
											}),
										resCmd,
										env);
								}
							}
						}
						return _Utils_Tuple3(
							$author$project$Page$TypeShortWord$reset(model),
							$author$project$Page$TypeShortWord$shuffleWords(readyData.shortWords),
							env);
				}
			case 'StartTime':
				var time = msg.a;
				var _v12 = model.state;
				switch (_v12.$) {
					case 'Init':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Error':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					default:
						var readyData = _v12.a;
						var _v13 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
						if (_v13.$ === 'Nothing') {
							return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
						} else {
							var word = _v13.a;
							var newWord = _Utils_update(
								word,
								{startTime: time});
							var newWords = A2($author$project$Page$TypeShortWord$updateCurrentWord, newWord, readyData.customTypingWords);
							return _Utils_Tuple3(
								_Utils_update(
									model,
									{
										state: $author$project$Page$TypeShortWord$Ready(
											_Utils_update(
												readyData,
												{customTypingWords: newWords}))
									}),
								$elm$core$Platform$Cmd$none,
								env);
						}
				}
			case 'FinishTime':
				var time = msg.a;
				var _v14 = model.state;
				switch (_v14.$) {
					case 'Init':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					case 'Error':
						return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
					default:
						var readyData = _v14.a;
						var _v15 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
						if (_v15.$ === 'Nothing') {
							return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
						} else {
							var word = _v15.a;
							var pWords = readyData.customTypingWords;
							var newWord = _Utils_update(
								word,
								{finishTime: time});
							var newWords = _Utils_update(
								pWords,
								{
									finish: _Utils_ap(
										pWords.finish,
										_List_fromArray(
											[newWord])),
									rest: A2($elm$core$List$drop, 1, pWords.rest)
								});
							return _Utils_Tuple3(
								_Utils_update(
									model,
									{
										state: $author$project$Page$TypeShortWord$Ready(
											_Utils_update(
												readyData,
												{customTypingWords: newWords}))
									}),
								$elm$core$Platform$Cmd$none,
								env);
						}
				}
			case 'DeleteWord':
				return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
			case 'ReceiveDeleteWord':
				if (msg.a.$ === 'Ok') {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: str}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'エラー'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'RegistRanking':
				return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
			case 'ReceiveRegistRanking':
				if (msg.a.$ === 'Ok') {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: str}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var str = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'エラー'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'GetRanking':
				return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
			case 'ReceiveGetRanking':
				if (msg.a.$ === 'Ok') {
					var lwr = msg.a.a;
					return _Utils_Tuple3(model, $elm$core$Platform$Cmd$none, env);
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{notice: 'ランキング取得失敗'}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'Tick':
				var newTimer = msg.a;
				return _Utils_Tuple3(
					A2($author$project$Page$TypeShortWord$setCountDownTimer, newTimer, model),
					$elm$core$Platform$Cmd$none,
					env);
			default:
				var newTimer = msg.a;
				return _Utils_Tuple3(
					A2($author$project$Page$TypeShortWord$setRhythmTimer, newTimer, model),
					function () {
						var _v16 = $author$project$CountDown$getState(newTimer);
						if (_v16.$ === 'Zero') {
							return A2($elm$core$Task$perform, $author$project$Page$TypeShortWord$StartTime, $elm$time$Time$now);
						} else {
							return $elm$core$Platform$Cmd$none;
						}
					}(),
					env);
		}
	});
var $author$project$Page$User$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Page$User$ReceiveChangeUser = function (a) {
	return {$: 'ReceiveChangeUser', a: a};
};
var $author$project$Page$User$ReceiveNewUserID = function (a) {
	return {$: 'ReceiveNewUserID', a: a};
};
var $author$project$Page$User$ReceiveReName = function (a) {
	return {$: 'ReceiveReName', a: a};
};
var $author$project$Page$User$Success = function (a) {
	return {$: 'Success', a: a};
};
var $author$project$Page$User$Waiting = function (a) {
	return {$: 'Waiting', a: a};
};
var $author$project$API$changeUser = F2(
	function (msg, user) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					$author$project$Data$User$encodeUser(user)),
				expect: A2($elm$http$Http$expectJson, msg, $author$project$Data$User$userDecoder),
				url: $author$project$API$host + ($author$project$API$api + '?changeUser')
			});
	});
var $author$project$API$reName = F2(
	function (msg, user) {
		return $elm$http$Http$post(
			{
				body: $elm$http$Http$jsonBody(
					$author$project$Data$User$encodeUser(user)),
				expect: A2($elm$http$Http$expectJson, msg, $author$project$Data$User$userDecoder),
				url: $author$project$API$host + ($author$project$API$api + '?modifyUser')
			});
	});
var $author$project$Page$User$update = F3(
	function (msg, model, env) {
		var pUser = model.user;
		switch (msg.$) {
			case 'InputName':
				var name = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							user: _Utils_update(
								pUser,
								{name: name})
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'InputID':
				var id = msg.a;
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							user: _Utils_update(
								pUser,
								{id: id})
						}),
					$elm$core$Platform$Cmd$none,
					env);
			case 'GetNewUserID':
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$User$Waiting('取得中')
						}),
					$author$project$API$getNewUserID($author$project$Page$User$ReceiveNewUserID),
					env);
			case 'ReceiveNewUserID':
				if (msg.a.$ === 'Err') {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Error('取得失敗')
							}),
						$elm$core$Platform$Cmd$none,
						env);
				} else {
					var user = msg.a.a;
					var newEnv = _Utils_update(
						env,
						{user: user});
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Success('新ID取得完了'),
								user: user
							}),
						$author$project$Ports$saveUser(
							$author$project$Data$User$encodeUser(user)),
						newEnv);
				}
			case 'ReName':
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$User$Waiting('名前変更中')
						}),
					A2($author$project$API$reName, $author$project$Page$User$ReceiveReName, model.user),
					env);
			case 'ReceiveReName':
				if (msg.a.$ === 'Ok') {
					var user = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Success('名前変更完了'),
								user: user
							}),
						$author$project$Ports$saveUser(
							$author$project$Data$User$encodeUser(user)),
						_Utils_update(
							env,
							{user: user}));
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Error('名前変更失敗')
							}),
						$elm$core$Platform$Cmd$none,
						env);
				}
			case 'ChangeUser':
				return _Utils_Tuple3(
					_Utils_update(
						model,
						{
							state: $author$project$Page$User$Waiting('ID切り替え中')
						}),
					A2($author$project$API$changeUser, $author$project$Page$User$ReceiveChangeUser, model.user),
					env);
			default:
				if (msg.a.$ === 'Ok') {
					var user = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Success('ID切り替え完了'),
								user: user
							}),
						$author$project$Ports$saveUser(
							$author$project$Data$User$encodeUser(user)),
						_Utils_update(
							env,
							{user: user}));
				} else {
					var e = msg.a.a;
					return _Utils_Tuple3(
						_Utils_update(
							model,
							{
								state: $author$project$Page$User$Error('ID切り替え失敗')
							}),
						$elm$core$Platform$Cmd$none,
						env);
				}
		}
	});
var $author$project$Page$WordList$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Page$WordList$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$Page$WordList$update = F3(
	function (msg, model, env) {
		if (msg.a.$ === 'Ok') {
			var wl = msg.a.a;
			return _Utils_Tuple3(
				_Utils_update(
					model,
					{
						state: $author$project$Page$WordList$Loaded(wl)
					}),
				$elm$core$Platform$Cmd$none,
				env);
		} else {
			var e = msg.a.a;
			return _Utils_Tuple3(
				_Utils_update(
					model,
					{
						state: $author$project$Page$WordList$Error(e)
					}),
				$elm$core$Platform$Cmd$none,
				env);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var pEnv = model.env;
		var _v0 = _Utils_Tuple2(msg, model.page);
		_v0$13:
		while (true) {
			switch (_v0.a.$) {
				case 'RestoreUser':
					var data = _v0.a.a;
					var res = A2($elm$json$Json$Decode$decodeValue, $author$project$Data$User$userDecoder, data);
					if (res.$ === 'Ok') {
						var user = res.a;
						var newEnv = _Utils_update(
							pEnv,
							{user: user});
						var newModel = _Utils_update(
							model,
							{env: newEnv});
						return A2($author$project$Main$goTo, newEnv.url, newModel);
					} else {
						return _Utils_Tuple2(
							model,
							$author$project$API$getNewUserID($author$project$Main$ReceiveNewUserID));
					}
				case 'ReceiveNewUserID':
					if (_v0.a.a.$ === 'Ok') {
						var user = _v0.a.a.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: _Utils_update(
										pEnv,
										{user: user})
								}),
							$author$project$Ports$saveUser(
								$author$project$Data$User$encodeUser(user)));
					} else {
						var e = _v0.a.a.a;
						var errUser = A2($author$project$Data$User$User, 'err_id', 'err_name');
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: _Utils_update(
										pEnv,
										{user: errUser})
								}),
							$elm$core$Platform$Cmd$none);
					}
				case 'LinkClicked':
					var urlRequest = _v0.a.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							A2(
								$elm$browser$Browser$Navigation$pushUrl,
								pEnv.key,
								$elm$url$Url$toString(url)));
					} else {
						var href = urlRequest.a;
						return _Utils_Tuple2(
							model,
							$elm$browser$Browser$Navigation$load(href));
					}
				case 'UrlChanged':
					var url = _v0.a.a;
					return A2($author$project$Main$goTo, url, model);
				case 'TopMsg':
					if (_v0.b.$ === 'TopPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v3 = A3($author$project$Page$Top$update, subMsg, subModel, model.env);
						var newModel = _v3.a;
						var topCmd = _v3.b;
						var newEnv = _v3.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$TopPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$TopMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'UserMsg':
					if (_v0.b.$ === 'UserPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v4 = A3($author$project$Page$User$update, subMsg, subModel, model.env);
						var newModel = _v4.a;
						var topCmd = _v4.b;
						var newEnv = _v4.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$UserPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$UserMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'PostLongWordMsg':
					if (_v0.b.$ === 'PostLongWordPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v5 = A3($author$project$Page$PostLongWord$update, subMsg, subModel, model.env);
						var newModel = _v5.a;
						var topCmd = _v5.b;
						var newEnv = _v5.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$PostLongWordPage(newModel),
									postLongWordPageCache: $elm$core$Maybe$Just(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$PostLongWordMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'WordListMsg':
					if (_v0.b.$ === 'WordListPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v6 = A3($author$project$Page$WordList$update, subMsg, subModel, model.env);
						var newModel = _v6.a;
						var topCmd = _v6.b;
						var newEnv = _v6.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$WordListPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$WordListMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'TypeLongWordMsg':
					if (_v0.b.$ === 'TypeLongWordPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v7 = A3($author$project$Page$TypeLongWord$update, subMsg, subModel, model.env);
						var newModel = _v7.a;
						var topCmd = _v7.b;
						var newEnv = _v7.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$TypeLongWordPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeLongWordMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'TypeAnyMsg':
					if (_v0.b.$ === 'TypeAnyPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v8 = A3($author$project$Page$TypeAny$update, subMsg, subModel, model.env);
						var newModel = _v8.a;
						var topCmd = _v8.b;
						var newEnv = _v8.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$TypeAnyPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeAnyMsg, topCmd));
					} else {
						break _v0$13;
					}
				case 'EtcMsg':
					if (_v0.b.$ === 'EtcPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v9 = A3($author$project$Page$Etc$update, subMsg, subModel, model.env);
						var newModel = _v9.a;
						var topCmd = _v9.b;
						var newEnv = _v9.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$EtcPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$EtcMsg, topCmd));
					} else {
						break _v0$13;
					}
				default:
					if (_v0.b.$ === 'TypeShortWordPage') {
						var subMsg = _v0.a.a;
						var subModel = _v0.b.a;
						var _v10 = A3($author$project$Page$TypeShortWord$update, subMsg, subModel, model.env);
						var newModel = _v10.a;
						var topCmd = _v10.b;
						var newEnv = _v10.c;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									env: newEnv,
									page: $author$project$Main$TypeShortWordPage(newModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$TypeShortWordMsg, topCmd));
					} else {
						break _v0$13;
					}
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Page$Etc$viewLink = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('./#typeAny')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('何でもいいタイピング')
					])),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('./#typeShortWord')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('短文')
					]))
			]));
};
var $author$project$Page$Etc$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('element-panel')
			]),
		_List_fromArray(
			[
				$author$project$Page$Etc$viewLink(model)
			]));
};
var $author$project$Page$PostLongWord$BackEdit = {$: 'BackEdit'};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $author$project$Page$PostLongWord$InputTitle = function (a) {
	return {$: 'InputTitle', a: a};
};
var $author$project$Page$PostLongWord$InputWordForInput = function (a) {
	return {$: 'InputWordForInput', a: a};
};
var $author$project$Page$PostLongWord$InputWordForView = function (a) {
	return {$: 'InputWordForView', a: a};
};
var $author$project$Page$PostLongWord$Post = {$: 'Post'};
var $author$project$Page$PostLongWord$TryType = {$: 'TryType'};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$autofocus = $elm$html$Html$Attributes$boolProperty('autofocus');
var $elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Data$LongWord$validate = function (rLW) {
	var lw = $author$project$Data$LongWord$trim(rLW);
	var lenWV = $elm$core$String$length(lw.wordForView);
	var lenWI = $elm$core$String$length(lw.wordForInput);
	var lenT = $elm$core$String$length(lw.title);
	var c4 = _Utils_eq(lenWV, lenWI);
	var c3 = (0 < lenWI) && (lenWI < 10000);
	var c2 = (0 < lenWV) && (lenWV < 10000);
	var c1 = (0 < lenT) && (lenT < 150);
	return A2(
		$elm$core$List$all,
		function (n) {
			return n;
		},
		_List_fromArray(
			[c1, c2, c3, c4]));
};
var $author$project$Page$PostLongWord$invalidRegistButton = function (model) {
	var c2 = !$author$project$Data$LongWord$validate(model.lw);
	var c1 = function () {
		var _v0 = model.state;
		_v0$2:
		while (true) {
			if (_v0.$ === 'Edit') {
				switch (_v0.a.$) {
					case 'Waiting':
						return true;
					case 'Success':
						return true;
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return false;
	}();
	return c1 || c2;
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Page$PostLongWord$viewState = function (model) {
	var _v0 = model.state;
	if (_v0.$ === 'Edit') {
		switch (_v0.a.$) {
			case 'Init':
				var _v1 = _v0.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('')
						]));
			case 'Waiting':
				var str = _v0.a.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			case 'Error':
				var str = _v0.a.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			default:
				var str = _v0.a.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
		}
	} else {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('')
				]));
	}
};
var $author$project$Page$PostLongWord$viewPostForm = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('お題名'),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Page$PostLongWord$InputTitle),
								$elm$html$Html$Attributes$autofocus(true),
								$elm$html$Html$Attributes$placeholder('お題名'),
								$elm$html$Html$Attributes$value(model.lw.title)
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('入力用ワード'),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Page$PostLongWord$InputWordForInput),
								$elm$html$Html$Attributes$placeholder('入力用ワード'),
								$elm$html$Html$Attributes$value(model.lw.wordForInput),
								$elm$html$Html$Attributes$cols(60),
								$elm$html$Html$Attributes$rows(10)
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('表示用ワード'),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Page$PostLongWord$InputWordForView),
								$elm$html$Html$Attributes$placeholder('表示用ワード'),
								$elm$html$Html$Attributes$value(model.lw.wordForView),
								$elm$html$Html$Attributes$cols(60),
								$elm$html$Html$Attributes$rows(10)
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$disabled(
						$author$project$Page$PostLongWord$invalidRegistButton(model)),
						$elm$html$Html$Events$onClick($author$project$Page$PostLongWord$TryType)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('テストプレイ')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$disabled(
						$author$project$Page$PostLongWord$invalidRegistButton(model)),
						$elm$html$Html$Events$onClick($author$project$Page$PostLongWord$Post)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('投稿')
					])),
				$author$project$Page$PostLongWord$viewState(model)
			]));
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				_Utils_chr('0'),
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				$elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					$elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _v0.a.a.valueOf()) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $author$project$Page$TypeLongWord$scoreToUsefulScore = function (score) {
	var time = score.time / 1000;
	var kpm = score.keys / (time / 60);
	var ac = ((score.keys - score.miss) / score.keys) * 100;
	return {
		accuracy: A2($myrho$elm_round$Round$round, 2, ac),
		kpm: A2($myrho$elm_round$Round$round, 2, kpm),
		miss: $elm$core$String$fromInt(score.miss),
		time: A2($myrho$elm_round$Round$round, 2, time)
	};
};
var $author$project$Page$TypeLongWord$viewScore = function (mscore) {
	if (mscore.$ === 'Just') {
		var score = mscore.a;
		var us = $author$project$Page$TypeLongWord$scoreToUsefulScore(score);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('typing-score__body')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(us.time + '秒, '),
					$elm$html$Html$text(us.miss + 'ミス, '),
					$elm$html$Html$text(us.accuracy + '%, '),
					$elm$html$Html$text(us.kpm + '打/分')
				]));
	} else {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	}
};
var $author$project$Page$TypeLongWord$viewBestScore = function (mscore) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('typing-score')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('typing-score__title')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('自己ベスト')
					])),
				$author$project$Page$TypeLongWord$viewScore(mscore)
			]));
};
var $author$project$Page$TypeLongWord$viewScoreHistory = function (scoreList) {
	var sl = A2(
		$elm$core$List$map,
		function (n) {
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Page$TypeLongWord$viewScore(
						$elm$core$Maybe$Just(n))
					]));
		},
		scoreList);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('typing-score-history')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('typing-score-history__title')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'スコア履歴(' + ($elm$core$String$fromInt(
							$elm$core$List$length(sl)) + ')'))
					])),
				A2($elm$html$Html$ul, _List_Nil, sl)
			]));
};
var $author$project$Page$TypeLongWord$calcAccuracy = F2(
	function (miss, keys) {
		var ac = ((keys - miss) / keys) * 100;
		return A2($myrho$elm_round$Round$round, 2, ac);
	});
var $author$project$Page$TypeLongWord$calcKpm = F3(
	function (startTime, finishTime, keys) {
		var st = $elm$time$Time$posixToMillis(startTime);
		var ft = $elm$time$Time$posixToMillis(finishTime);
		var time = ft - st;
		var kpm = keys / (time / (1000 * 60));
		return A2($myrho$elm_round$Round$round, 2, kpm);
	});
var $author$project$Page$TypeLongWord$calcSec = F2(
	function (startTime, finishTime) {
		var st = $elm$time$Time$posixToMillis(startTime);
		var ft = $elm$time$Time$posixToMillis(finishTime);
		var time = (ft - st) / 1000;
		return A2($myrho$elm_round$Round$round, 2, time);
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$Page$TypeLongWord$viewText = F2(
	function (model, typingData) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					'typing-form' + (model.missed ? ' typing-form__missed' : ''))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('typing-form__body')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('typing-form__words')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('typing-form__fixed')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A3(
												$elm$core$String$replace,
												'ζ',
												'',
												A2(
													$elm$core$String$left,
													$elm$core$String$length(typingData.fixedWords),
													model.wordForView)))
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('typing-form__rest ')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A3(
												$elm$core$String$replace,
												'ζ',
												'',
												A2(
													$elm$core$String$dropLeft,
													$elm$core$String$length(typingData.fixedWords),
													model.wordForView)))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('typing-form__input')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									($elm$core$String$length(model.inputHistory) > 30) ? A2($elm$core$String$right, 30, model.inputHistory) : model.inputHistory)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									_Utils_eq(model.typingState, $author$project$Page$TypeLongWord$Finish) ? 'typing-form__state__finish' : 'typing-form__state')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									'ミス数: ' + ($elm$core$String$fromInt(model.miss) + (_Utils_eq(model.typingState, $author$project$Page$TypeLongWord$Finish) ? (', ' + (A2(
										$author$project$Page$TypeLongWord$calcAccuracy,
										model.miss,
										$elm$core$String$length(model.inputHistory)) + ('%, ' + (A2($author$project$Page$TypeLongWord$calcSec, model.startTime, model.finishTime) + ('秒, ' + (A3(
										$author$project$Page$TypeLongWord$calcKpm,
										model.startTime,
										model.finishTime,
										$elm$core$String$length(model.inputHistory)) + ('打/分, ' + '    Finish'))))))) : '')))
								]))
						]))
				]));
	});
var $author$project$Page$TypeLongWord$viewTyping = function (model) {
	var _v0 = _Utils_Tuple2(model.modelState, model.typingData);
	_v0$2:
	while (true) {
		switch (_v0.a.$) {
			case 'Init':
				var _v1 = _v0.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('準備中')
						]));
			case 'Error':
				var str = _v0.a.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('エラー: ' + str)
						]));
			case 'Loaded':
				if (_v0.b.$ === 'Nothing') {
					break _v0$2;
				} else {
					var typingData = _v0.b.a;
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$Page$TypeLongWord$viewText, model, typingData),
								$author$project$Page$TypeLongWord$viewBestScore(model.bestScore),
								$author$project$Page$TypeLongWord$viewScoreHistory(model.scoreHistory)
							]));
				}
			default:
				if (_v0.b.$ === 'Nothing') {
					break _v0$2;
				} else {
					var typingData = _v0.b.a;
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$Page$TypeLongWord$viewText, model, typingData),
								$author$project$Page$TypeLongWord$viewBestScore(model.bestScore),
								$author$project$Page$TypeLongWord$viewScoreHistory(model.scoreHistory)
							]));
				}
		}
	}
	var _v2 = _v0.b;
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text('データ解釈失敗')
			]));
};
var $author$project$Page$PostLongWord$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				function () {
					var _v0 = model.state;
					if (_v0.$ === 'Edit') {
						var es = _v0.a;
						return _List_fromArray(
							[
								$author$project$Page$PostLongWord$viewPostForm(model)
							]);
					} else {
						var subModel = _v0.a;
						return _List_fromArray(
							[
								$author$project$Page$TypeLongWord$viewTyping(subModel),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Page$PostLongWord$BackEdit)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('編集に戻る')
									]))
							]);
					}
				}()),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('normal-sentence')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h3,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('入力用ワードについて')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('英数字記号などは半角で、それ以外はひらがなでお願いします。「、」「。」『「」』はそのままでもOKです。'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('例: じぶん[さんきゅー.bye]')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('normal-sentence')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h3,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('表示用ワードについて')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('漢字のように1文字が複数文字を表す場合、\"ζ\"を前に置き文字数を合わせてください。'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('例: 自ζ分「サンキュー。bye」')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('normal-sentence')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h3,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('補足')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('1文字目の\"じ\"が\"自\"に相当し、2文字目の\"ぶ\"が\"ζ\"に、3文字目の\"ん\"が\"分\"に、といった具合です。'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('例: \"わたし\"=\"ζζ私\"'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('例: \"じ\"=\"自\"'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('入力用ワードと表示用ワードで文字数が一致しない場合は投稿ボタンが押せません。'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('投稿したお題はタイピングページで後で削除することができます。')
									]))
							]))
					]))
			]));
};
var $author$project$Page$Top$viewState = function (model) {
	var _v0 = model.state;
	switch (_v0.$) {
		case 'Init':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Loading...')
					]));
		case 'Loaded':
			var str = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
		case 'Success':
			var str = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
		case 'Error':
			var str = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
		default:
			var str = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
	}
};
var $author$project$Page$Top$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text('Hello'),
				$author$project$Page$Top$viewState(model)
			]));
};
var $author$project$Page$TypeAny$kpmToString = function (kpm) {
	return A2($myrho$elm_round$Round$round, 2, kpm);
};
var $author$project$Page$TypeAny$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('element-panel')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('typing-form')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('typing-form__body')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('typing-form__input')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(model.input)
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('wrap-lines')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('wrap-line')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('wrap-line__left')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('best: ')
									])),
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('wrap-line__right')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Page$TypeAny$kpmToString(model.bestKPM))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('wrap-line')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('wrap-line__left')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('kpm: ')
									])),
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('wrap-line__right')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Page$TypeAny$kpmToString(model.nowKPM))
									]))
							]))
					]))
			]));
};
var $author$project$Page$TypeLongWord$DeleteWord = {$: 'DeleteWord'};
var $author$project$Page$TypeLongWord$RegistRanking = {$: 'RegistRanking'};
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Page$TypeLongWord$viewRanking = function (score) {
	var us = $author$project$Page$TypeLongWord$scoreToUsefulScore(score.score);
	return A2(
		$elm$html$Html$tr,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				score.your ? 'ranking-table__you' : '')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(score.rank))
					])),
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(score.name)
					])),
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(us.time)
					])),
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(us.miss)
					])),
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(us.accuracy)
					])),
				A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(us.kpm)
					]))
			]));
};
var $author$project$Page$TypeLongWord$viewRankingList = function (lwr) {
	if (lwr.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		var ranking = lwr.a;
		var rl = A2(
			$elm$core$List$map,
			function (n) {
				return $author$project$Page$TypeLongWord$viewRanking(n);
			},
			ranking);
		return A2(
			$elm$html$Html$table,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('ranking-table')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$thead,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('順位')
								])),
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('名前')
								])),
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('タイム')
								])),
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('miss')
								])),
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('正確率')
								])),
							A2(
							$elm$html$Html$th,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('kpm')
								]))
						])),
					A2($elm$html$Html$tbody, _List_Nil, rl)
				]));
	}
};
var $author$project$Page$TypeLongWord$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				_List_fromArray(
					[
						$author$project$Page$TypeLongWord$viewTyping(model)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				function () {
					var _v0 = model.modelState;
					if (_v0.$ === 'Loaded') {
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Page$TypeLongWord$RegistRanking)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('ランキング登録')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Page$TypeLongWord$GetRanking)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('ランキング取得')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Page$TypeLongWord$DeleteWord)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('ワード削除')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(model.notice)
									]))
							]);
					} else {
						return _List_fromArray(
							[
								$elm$html$Html$text(model.notice)
							]);
					}
				}()),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				function () {
					var _v1 = model.modelState;
					if (_v1.$ === 'Loaded') {
						return _List_fromArray(
							[
								$author$project$Page$TypeLongWord$viewRankingList(model.ranking)
							]);
					} else {
						return _List_fromArray(
							[
								$elm$html$Html$text('')
							]);
					}
				}())
			]));
};
var $author$project$CountDown$getSecond = function (_v0) {
	var timer = _v0.a;
	return $elm$core$Basics$round(timer.time / 1000);
};
var $author$project$Page$TypeShortWord$calcAccuracy = F2(
	function (miss, keys) {
		return keys / (keys + miss);
	});
var $author$project$Page$TypeShortWord$calcKpm = F2(
	function (millis, keys) {
		return keys / (millis / (1000 * 60));
	});
var $author$project$Page$TypeShortWord$calcScore = F3(
	function (millis, keys, miss) {
		return keys / ((millis + ((miss / 3) * 1000)) / (1000 * 60));
	});
var $GoldentTuft$elm_japanese_typing$Typing$getHistory = function (_v0) {
	var data = _v0.a;
	return data.history;
};
var $author$project$Page$TypeShortWord$getSumOfInput = function (words) {
	var f = F2(
		function (w, sum) {
			return sum + $elm$core$String$length(
				$GoldentTuft$elm_japanese_typing$Typing$getHistory(w.typingData));
		});
	return A3($elm$core$List$foldl, f, 0, words);
};
var $author$project$Page$TypeShortWord$getSumOfMillis = function (words) {
	var f = F2(
		function (w, sum) {
			return sum + ($elm$time$Time$posixToMillis(w.finishTime) - $elm$time$Time$posixToMillis(w.startTime));
		});
	return A3($elm$core$List$foldl, f, 0, words);
};
var $author$project$Page$TypeShortWord$round2 = function (a) {
	return A2($myrho$elm_round$Round$round, 2, a);
};
var $author$project$Page$TypeShortWord$viewAccuracy = function (ac) {
	return $elm$html$Html$text(
		'正確率: ' + ($author$project$Page$TypeShortWord$round2(ac * 100) + '%'));
};
var $author$project$Page$TypeShortWord$viewKpm = function (kpm) {
	return $elm$html$Html$text(
		'kpm: ' + $author$project$Page$TypeShortWord$round2(kpm));
};
var $author$project$Page$TypeShortWord$viewScore = function (score) {
	return $elm$html$Html$text(
		'score: ' + (A2($myrho$elm_round$Round$round, 0, score) + 'pt'));
};
var $author$project$Page$TypeShortWord$viewDetails = function (words) {
	var millis = $author$project$Page$TypeShortWord$getSumOfMillis(words.finish);
	var keys = $author$project$Page$TypeShortWord$getSumOfInput(words.finish);
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Page$TypeShortWord$viewKpm(
				A2($author$project$Page$TypeShortWord$calcKpm, millis, keys)),
				$elm$html$Html$text(', '),
				$author$project$Page$TypeShortWord$viewAccuracy(
				A2($author$project$Page$TypeShortWord$calcAccuracy, words.miss, keys)),
				$elm$html$Html$text(', '),
				$elm$html$Html$text(
				'miss: ' + $elm$core$String$fromInt(words.miss)),
				$elm$html$Html$text(', '),
				$author$project$Page$TypeShortWord$viewScore(
				A3($author$project$Page$TypeShortWord$calcScore, millis, keys, words.miss))
			]));
};
var $GoldentTuft$elm_japanese_typing$Typing$getFixed = function (_v0) {
	var data = _v0.a;
	return data.fixedWords;
};
var $author$project$Page$TypeShortWord$getFixed = F2(
	function (wordForView, td) {
		var fixed = $GoldentTuft$elm_japanese_typing$Typing$getFixed(td);
		return A3(
			$elm$core$String$replace,
			'ζ',
			'',
			A2(
				$elm$core$String$left,
				$elm$core$String$length(fixed),
				wordForView));
	});
var $author$project$Page$TypeShortWord$getRest = F2(
	function (wordForView, td) {
		var fixed = $GoldentTuft$elm_japanese_typing$Typing$getFixed(td);
		return A3(
			$elm$core$String$replace,
			'ζ',
			'',
			A2(
				$elm$core$String$dropLeft,
				$elm$core$String$length(fixed),
				wordForView));
	});
var $author$project$Page$TypeShortWord$inputHistoryView = function (word) {
	var history1 = $GoldentTuft$elm_japanese_typing$Typing$getHistory(word.typingData);
	var history2 = ($elm$core$String$length(history1) > 15) ? A2($elm$core$String$right, 15, history1) : history1;
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('typing-form__input__history')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(history2)
			]));
};
var $author$project$Page$TypeShortWord$nbsp = '\u00A0';
var $GoldentTuft$elm_japanese_typing$Typing$dropHead = F2(
	function (head, str) {
		return A2($elm$core$String$startsWith, head, str) ? A2(
			$elm$core$String$dropLeft,
			$elm$core$String$length(head),
			str) : str;
	});
var $GoldentTuft$elm_japanese_typing$Typing$makeRomaji_ = F2(
	function (_v0, candidates) {
		makeRomaji_:
		while (true) {
			var data = _v0.a;
			var _v1 = data.state;
			switch (_v1.$) {
				case 'Finish':
					return $elm$core$Maybe$Just(
						$GoldentTuft$elm_japanese_typing$Typing$Data(data));
				case 'Miss':
					return $elm$core$Maybe$Nothing;
				default:
					if (!candidates.b) {
						return $elm$core$Maybe$Nothing;
					} else {
						var c = candidates.a;
						var cs = candidates.b;
						var input = A2($GoldentTuft$elm_japanese_typing$Typing$dropHead, data.convertBuf.inputBuffer, c.input);
						var _v3 = A2(
							$GoldentTuft$elm_japanese_typing$Typing$typeTo,
							input,
							$GoldentTuft$elm_japanese_typing$Typing$Data(data));
						var nd = _v3.a;
						var rest = function () {
							var _v5 = nd.convertBuf.tmpFixed;
							if (_v5.$ === 'Nothing') {
								return nd.restWords;
							} else {
								var r = _v5.a;
								return A2(
									$elm$core$String$dropLeft,
									$elm$core$String$length(r.output),
									data.restWords);
							}
						}();
						var nc = A2(
							$elm$core$List$filter,
							function (r) {
								return A2($elm$core$String$startsWith, r.output, rest);
							},
							data.rules);
						var _v4 = A2(
							$GoldentTuft$elm_japanese_typing$Typing$makeRomaji_,
							$GoldentTuft$elm_japanese_typing$Typing$Data(nd),
							nc);
						if (_v4.$ === 'Nothing') {
							var $temp$_v0 = $GoldentTuft$elm_japanese_typing$Typing$Data(data),
								$temp$candidates = cs;
							_v0 = $temp$_v0;
							candidates = $temp$candidates;
							continue makeRomaji_;
						} else {
							var rd = _v4.a;
							return $elm$core$Maybe$Just(rd);
						}
					}
			}
		}
	});
var $GoldentTuft$elm_japanese_typing$Typing$makeRomaji = function (_v0) {
	var data = _v0.a;
	var candidates = A2(
		$elm$core$List$filter,
		function (r) {
			return A2($elm$core$String$startsWith, r.output, data.restWords);
		},
		data.convertBuf.candidates);
	return A2(
		$elm$core$Maybe$map,
		function (fd) {
			return $GoldentTuft$elm_japanese_typing$Typing$getHistory(fd);
		},
		A2(
			$GoldentTuft$elm_japanese_typing$Typing$makeRomaji_,
			$GoldentTuft$elm_japanese_typing$Typing$Data(
				_Utils_update(
					data,
					{history: ''})),
			candidates));
};
var $author$project$Page$TypeShortWord$restRomajiView = F2(
	function (missed, data) {
		var romaji = A2(
			$elm$core$Maybe$withDefault,
			'',
			$GoldentTuft$elm_japanese_typing$Typing$makeRomaji(data));
		var rest = A2($elm$core$String$dropLeft, 1, romaji);
		var focused = A2($elm$core$String$left, 1, romaji);
		return A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('typing-form__input__focused')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(focused)
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('typing-form__input__rest')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(rest)
						]))
				]));
	});
var $author$project$Page$TypeShortWord$viewText = F2(
	function (words, word) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					'typing-form' + (words.missed ? ' typing-form__missed' : ''))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('typing-form__body')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('typing-form__words'),
									A2(
									$elm$html$Html$Attributes$style,
									'visibility',
									function () {
										var _v0 = $author$project$CountDown$getState(word.rhythmTimer);
										if (_v0.$ === 'Zero') {
											return 'visible';
										} else {
											return 'hidden';
										}
									}())
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('typing-form__fixed')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($author$project$Page$TypeShortWord$getFixed, word.wordForView, word.typingData))
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('typing-form__rest')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($author$project$Page$TypeShortWord$getRest, word.wordForView, word.typingData))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('typing-form__input'),
									A2(
									$elm$html$Html$Attributes$style,
									'visibility',
									function () {
										var _v1 = $author$project$CountDown$getState(word.rhythmTimer);
										if (_v1.$ === 'Zero') {
											return 'visible';
										} else {
											return 'hidden';
										}
									}())
								]),
							_List_fromArray(
								[
									$author$project$Page$TypeShortWord$inputHistoryView(word),
									A2($author$project$Page$TypeShortWord$restRomajiView, words.missed, word.typingData)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('typing-form__state')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									'ミス数:' + $elm$core$String$fromInt(words.miss)),
									$elm$html$Html$text(
									_Utils_ap(
										A2($elm$core$String$repeat, 10, $author$project$Page$TypeShortWord$nbsp),
										$elm$core$String$fromInt(
											$elm$core$List$length(words.finish) + 1))),
									$elm$html$Html$text(
									'/' + $elm$core$String$fromInt(
										$elm$core$List$length(words.finish) + $elm$core$List$length(words.rest)))
								]))
						]))
				]));
	});
var $author$project$Page$TypeShortWord$viewTyping = function (model) {
	var _v0 = model.state;
	switch (_v0.$) {
		case 'Init':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('init viewTyping')
					]));
		case 'Error':
			var str = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('error(' + (str + ' viewTyping'))
					]));
		default:
			var readyData = _v0.a;
			var _v1 = $author$project$Page$TypeShortWord$getCurrentWord(readyData.customTypingWords);
			if (_v1.$ === 'Nothing') {
				return $author$project$Page$TypeShortWord$viewDetails(readyData.customTypingWords);
			} else {
				var word = _v1.a;
				return A2($author$project$Page$TypeShortWord$viewText, readyData.customTypingWords, word);
			}
	}
};
var $author$project$Page$TypeShortWord$wordsSumaryView = function (words) {
	return A2(
		$elm$html$Html$p,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(
				function (a) {
					return a + 'ワード';
				}(
					$elm$core$String$fromInt(
						$elm$core$List$length(words.words))))
			]));
};
var $author$project$Page$TypeShortWord$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				_List_fromArray(
					[
						function () {
						var _v0 = $author$project$Page$TypeShortWord$getCountDownTimer(model);
						if (_v0.$ === 'Nothing') {
							return $elm$html$Html$text(model.notice);
						} else {
							var timer = _v0.a;
							var _v1 = $author$project$CountDown$getState(timer);
							switch (_v1.$) {
								case 'Stop':
									return $elm$html$Html$text('カウントダウンストップエラー');
								case 'Error':
									return $elm$html$Html$text('カウントダウンエラー');
								case 'Ready':
									return $elm$html$Html$text('スペースキーでスタートです');
								case 'Tick':
									return $elm$html$Html$text(
										$elm$core$String$fromInt(
											$author$project$CountDown$getSecond(timer)));
								default:
									return $author$project$Page$TypeShortWord$viewTyping(model);
							}
						}
					}()
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('element-panel')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Escキーでリセットです。')
							])),
						function () {
						var _v2 = model.state;
						if (_v2.$ === 'Ready') {
							var data = _v2.a;
							return A2($elm$html$Html$Lazy$lazy, $author$project$Page$TypeShortWord$wordsSumaryView, data.shortWords);
						} else {
							return A2($elm$html$Html$p, _List_Nil, _List_Nil);
						}
					}()
					]))
			]));
};
var $author$project$Page$User$ChangeUser = {$: 'ChangeUser'};
var $author$project$Page$User$GetNewUserID = {$: 'GetNewUserID'};
var $author$project$Page$User$InputID = function (a) {
	return {$: 'InputID', a: a};
};
var $author$project$Page$User$InputName = function (a) {
	return {$: 'InputName', a: a};
};
var $author$project$Page$User$ReName = {$: 'ReName'};
var $author$project$Page$User$viewState = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				function () {
				var _v0 = model.state;
				switch (_v0.$) {
					case 'Init':
						return $elm$html$Html$text('');
					case 'Success':
						var str = _v0.a;
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(str)
								]));
					case 'Error':
						var str = _v0.a;
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(str)
								]));
					default:
						var str = _v0.a;
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(str)
								]));
				}
			}()
			]));
};
var $author$project$Page$User$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('element-panel')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('ユーザー名'),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Page$User$InputName),
								$elm$html$Html$Attributes$autofocus(false),
								$elm$html$Html$Attributes$value(model.user.name)
							]),
						_List_Nil),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$disabled(
								$elm$core$String$isEmpty(model.user.name)),
								$elm$html$Html$Events$onClick($author$project$Page$User$ReName)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('変更')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('ユーザーID'),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Page$User$InputID),
								$elm$html$Html$Attributes$autofocus(false),
								$elm$html$Html$Attributes$value(model.user.id)
							]),
						_List_Nil),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$disabled(
								$elm$core$String$isEmpty(model.user.id)),
								$elm$html$Html$Events$onClick($author$project$Page$User$ChangeUser)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('切り替え')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Page$User$GetNewUserID)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('再取得')
							]))
					])),
				$author$project$Page$User$viewState(model)
			]));
};
var $author$project$Page$WordList$viewWordSummary = function (ws) {
	return A2(
		$elm$html$Html$li,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('word-summary')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(' word-summary__word'),
						$elm$html$Html$Attributes$href(
						'./?id=' + ($elm$core$String$fromInt(ws.id) + '#typeLongWord'))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(ws.title)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('word-summary__by')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('by\u00A0' + ws.userName)
					]))
			]));
};
var $author$project$Page$WordList$viewWordList = function (wl) {
	return A2(
		$elm$html$Html$ul,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('word-list')
			]),
		A2($elm$core$List$map, $author$project$Page$WordList$viewWordSummary, wl));
};
var $author$project$Page$WordList$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('element-panel')
			]),
		_List_fromArray(
			[
				function () {
				var _v0 = model.state;
				switch (_v0.$) {
					case 'Init':
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Loading WordList...')
								]));
					case 'Loaded':
						var wl = _v0.a;
						return $author$project$Page$WordList$viewWordList(wl);
					default:
						var e = _v0.a;
						return $elm$html$Html$text('ロード失敗');
				}
			}()
			]));
};
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $author$project$Main$viewHeadder = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$nav,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('header-nav')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('./')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('header-nav__top')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('タイピング部(β)')
												]))
										]))
								])),
							A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('./#list')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('お題リスト')
												]))
										]))
								])),
							A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('./#post')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('お題投稿')
												]))
										]))
								])),
							A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('./#user')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('ユーザー')
												]))
										]))
								])),
							A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('./#etc')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('etc')
												]))
										]))
								]))
						]))
				]))
		]));
var $author$project$Main$viewNotFound = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			$elm$html$Html$text('NotFound')
		]));
var $author$project$Main$view = function (model) {
	return {
		body: _List_fromArray(
			[
				$author$project$Main$viewHeadder,
				function () {
				var _v0 = model.page;
				switch (_v0.$) {
					case 'NotFound':
						return $author$project$Main$viewNotFound;
					case 'TopPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$TopMsg,
							$author$project$Page$Top$view(subModel));
					case 'UserPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$UserMsg,
							$author$project$Page$User$view(subModel));
					case 'PostLongWordPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$PostLongWordMsg,
							$author$project$Page$PostLongWord$view(subModel));
					case 'WordListPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$WordListMsg,
							$author$project$Page$WordList$view(subModel));
					case 'TypeLongWordPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$TypeLongWordMsg,
							$author$project$Page$TypeLongWord$view(subModel));
					case 'EtcPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$EtcMsg,
							$author$project$Page$Etc$view(subModel));
					case 'TypeAnyPage':
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$TypeAnyMsg,
							$author$project$Page$TypeAny$view(subModel));
					default:
						var subModel = _v0.a;
						return A2(
							$elm$html$Html$map,
							$author$project$Main$TypeShortWordMsg,
							$author$project$Page$TypeShortWord$view(subModel));
				}
			}()
			]),
		title: 'タイピング部(β)'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$UrlChanged, onUrlRequest: $author$project$Main$LinkClicked, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))({"versions":{"elm":"0.19.1"},"types":{"message":"Main.Msg","aliases":{"Url.Url":{"args":[],"type":"{ protocol : Url.Protocol, host : String.String, port_ : Maybe.Maybe Basics.Int, path : String.String, query : Maybe.Maybe String.String, fragment : Maybe.Maybe String.String }"},"Data.User.User":{"args":[],"type":"{ id : String.String, name : String.String }"},"Json.Decode.Value":{"args":[],"type":"Json.Encode.Value"},"Data.LongWord.LongWord":{"args":[],"type":"{ title : String.String, id : Basics.Int, wordForView : String.String, wordForInput : String.String }"},"API.LongWordRanking":{"args":[],"type":"List.List API.LongWordRankingScore"},"API.LongWordRankingScore":{"args":[],"type":"{ rank : Basics.Int, score : Data.LongWord.Score, name : String.String, your : Basics.Bool }"},"Data.LongWord.Score":{"args":[],"type":"{ time : Basics.Int, keys : Basics.Int, miss : Basics.Int }"},"Data.ShortWord.ShortWords":{"args":[],"type":"{ title : String.String, id : Basics.Int, words : List.List Data.ShortWord.Word }"},"Data.ShortWord.Word":{"args":[],"type":"{ wordForView : String.String, wordForInput : String.String }"},"API.WordList":{"args":[],"type":"List.List API.WordSummary"},"API.WordSummary":{"args":[],"type":"{ id : Basics.Int, title : String.String, userName : String.String }"}},"unions":{"Main.Msg":{"args":[],"tags":{"RestoreUser":["Json.Decode.Value"],"ReceiveNewUserID":["Result.Result Http.Error Data.User.User"],"LinkClicked":["Browser.UrlRequest"],"UrlChanged":["Url.Url"],"TopMsg":["Page.Top.Msg"],"UserMsg":["Page.User.Msg"],"PostLongWordMsg":["Page.PostLongWord.Msg"],"WordListMsg":["Page.WordList.Msg"],"TypeLongWordMsg":["Page.TypeLongWord.Msg"],"EtcMsg":["Page.Etc.Msg"],"TypeAnyMsg":["Page.TypeAny.Msg"],"TypeShortWordMsg":["Page.TypeShortWord.Msg"]}},"Http.Error":{"args":[],"tags":{"BadUrl":["String.String"],"Timeout":[],"NetworkError":[],"BadStatus":["Basics.Int"],"BadBody":["String.String"]}},"Basics.Int":{"args":[],"tags":{"Int":[]}},"Maybe.Maybe":{"args":["a"],"tags":{"Just":["a"],"Nothing":[]}},"Page.Etc.Msg":{"args":[],"tags":{"None":[]}},"Page.PostLongWord.Msg":{"args":[],"tags":{"InputTitle":["String.String"],"InputWordForInput":["String.String"],"InputWordForView":["String.String"],"Post":[],"ReceivePost":["Result.Result Http.Error String.String"],"TryType":[],"BackEdit":[],"TypeLongWordMsg":["Page.TypeLongWord.Msg"]}},"Page.Top.Msg":{"args":[],"tags":{"None":[]}},"Page.TypeAny.Msg":{"args":[],"tags":{"KeyDown":["String.String"],"TenSecond":["Time.Posix"],"OneSecond":["Time.Posix"]}},"Page.TypeLongWord.Msg":{"args":[],"tags":{"ReceiveLongWord":["Result.Result Http.Error Data.LongWord.LongWord"],"KeyDown":["String.String"],"StartTime":["Time.Posix"],"FinishTime":["Time.Posix"],"DeleteWord":[],"ReceiveDeleteWord":["Result.Result Http.Error String.String"],"RegistRanking":[],"ReceiveRegistRanking":["Result.Result Http.Error String.String"],"GetRanking":[],"ReceiveGetRanking":["Result.Result Http.Error API.LongWordRanking"]}},"Page.TypeShortWord.Msg":{"args":[],"tags":{"ReceiveShortWords":["Result.Result Http.Error Data.ShortWord.ShortWords"],"KeyDown":["String.String"],"StartTime":["Time.Posix"],"FinishTime":["Time.Posix"],"DeleteWord":[],"ReceiveDeleteWord":["Result.Result Http.Error String.String"],"RegistRanking":[],"ReceiveRegistRanking":["Result.Result Http.Error String.String"],"GetRanking":[],"ReceiveGetRanking":["Result.Result Http.Error API.LongWordRanking"],"ShuffleWords":["List.List Data.ShortWord.Word"],"Tick":["CountDown.Timer"],"TickRhythm":["CountDown.Timer"]}},"Page.User.Msg":{"args":[],"tags":{"InputName":["String.String"],"InputID":["String.String"],"ReceiveNewUserID":["Result.Result Http.Error Data.User.User"],"GetNewUserID":[],"ReName":[],"ReceiveReName":["Result.Result Http.Error Data.User.User"],"ChangeUser":[],"ReceiveChangeUser":["Result.Result Http.Error Data.User.User"]}},"Page.WordList.Msg":{"args":[],"tags":{"ReceiveAllWordList":["Result.Result Http.Error API.WordList"]}},"Url.Protocol":{"args":[],"tags":{"Http":[],"Https":[]}},"Result.Result":{"args":["error","value"],"tags":{"Ok":["value"],"Err":["error"]}},"String.String":{"args":[],"tags":{"String":[]}},"Browser.UrlRequest":{"args":[],"tags":{"Internal":["Url.Url"],"External":["String.String"]}},"Json.Encode.Value":{"args":[],"tags":{"Value":[]}},"Basics.Bool":{"args":[],"tags":{"True":[],"False":[]}},"List.List":{"args":["a"],"tags":{}},"Time.Posix":{"args":[],"tags":{"Posix":["Basics.Int"]}},"CountDown.Timer":{"args":[],"tags":{"Timer":["{ start : Basics.Bool, initialTime : Basics.Int, time : Basics.Int, interval : Basics.Int }"]}}}}})}});

//////////////////// HMR BEGIN ////////////////////

/*
  MIT License http://www.opensource.org/licenses/mit-license.php
  Original Author: Flux Xu @fluxxu
*/

/*
    A note about the environment that this code runs in...

    assumed globals:
        - `module` (from Node.js module system and webpack)

    assumed in scope after injection into the Elm IIFE:
        - `scope` (has an 'Elm' property which contains the public Elm API)
        - various functions defined by Elm which we have to hook such as `_Platform_initialize` and `_Scheduler_binding`
 */

if (module.hot) {
    (function () {
        "use strict";

        //polyfill for IE: https://github.com/fluxxu/elm-hot-loader/issues/16
        if (typeof Object.assign != 'function') {
            Object.assign = function (target) {
                'use strict';
                if (target == null) {
                    throw new TypeError('Cannot convert undefined or null to object');
                }

                target = Object(target);
                for (var index = 1; index < arguments.length; index++) {
                    var source = arguments[index];
                    if (source != null) {
                        for (var key in source) {
                            if (Object.prototype.hasOwnProperty.call(source, key)) {
                                target[key] = source[key];
                            }
                        }
                    }
                }
                return target;
            };
        }

        // Elm 0.19.1 introduced a '$' prefix at the beginning of the symbols it emits,
        // and we check for `Maybe.Just` because we expect it to be present in all Elm programs.
        var elmVersion;
        if (typeof elm$core$Maybe$Just !== 'undefined')
            elmVersion = '0.19.0';
        else if (typeof $elm$core$Maybe$Just !== 'undefined')
            elmVersion = '0.19.1';
        else
            throw new Error("Could not determine Elm version");

        function elmSymbol(symbol) {
            try {
                switch (elmVersion) {
                    case '0.19.0':
                        return eval(symbol);
                    case '0.19.1':
                        return eval('$' + symbol);
                    default:
                        throw new Error('Cannot resolve ' + symbol + '. Elm version unknown!')
                }
            } catch (e) {
                if (e instanceof ReferenceError) {
                    return undefined;
                } else {
                    throw e;
                }
            }
        }

        var instances = module.hot.data
            ? module.hot.data.instances || {}
            : {};
        var uid = module.hot.data
            ? module.hot.data.uid || 0
            : 0;

        if (Object.keys(instances).length === 0) {
            log("[elm-hot] Enabled");
        }

        var cancellers = [];

        // These 2 variables act as dynamically-scoped variables which are set only when the
        // Elm module's hooked init function is called.
        var initializingInstance = null;
        var swappingInstance = null;

        module.hot.accept();
        module.hot.dispose(function (data) {
            data.instances = instances;
            data.uid = uid;

            // Cleanup pending async tasks

            // First, make sure that no new tasks can be started until we finish replacing the code
            _Scheduler_binding = function () {
                return _Scheduler_fail(new Error('[elm-hot] Inactive Elm instance.'))
            };

            // Second, kill pending tasks belonging to the old instance
            if (cancellers.length) {
                log('[elm-hot] Killing ' + cancellers.length + ' running processes...');
                try {
                    cancellers.forEach(function (cancel) {
                        cancel();
                    });
                } catch (e) {
                    console.warn('[elm-hot] Kill process error: ' + e.message);
                }
            }
        });

        function log(message) {
            if (module.hot.verbose) {
                console.log(message)
            }
        }

        function getId() {
            return ++uid;
        }

        function findPublicModules(parent, path) {
            var modules = [];
            for (var key in parent) {
                var child = parent[key];
                var currentPath = path ? path + '.' + key : key;
                if ('init' in child) {
                    modules.push({
                        path: currentPath,
                        module: child
                    });
                } else {
                    modules = modules.concat(findPublicModules(child, currentPath));
                }
            }
            return modules;
        }

        function registerInstance(domNode, flags, path, portSubscribes, portSends) {
            var id = getId();

            var instance = {
                id: id,
                path: path,
                domNode: domNode,
                flags: flags,
                portSubscribes: portSubscribes,
                portSends: portSends,
                lastState: null // last Elm app state (root model)
            };

            return instances[id] = instance
        }

        function isFullscreenApp() {
            // Returns true if the Elm app will take over the entire DOM body.
            return typeof elmSymbol("elm$browser$Browser$application") !== 'undefined'
                || typeof elmSymbol("elm$browser$Browser$document") !== 'undefined';
        }

        function wrapDomNode(node) {
            // When embedding an Elm app into a specific DOM node, Elm will replace the provided
            // DOM node with the Elm app's content. When the Elm app is compiled normally, the
            // original DOM node is reused (its attributes and content changes, but the object
            // in memory remains the same). But when compiled using `--debug`, Elm will completely
            // destroy the original DOM node and instead replace it with 2 brand new nodes: one
            // for your Elm app's content and the other for the Elm debugger UI. In this case,
            // if you held a reference to the DOM node provided for embedding, it would be orphaned
            // after Elm module initialization.
            //
            // So in order to make both cases consistent and isolate us from changes in how Elm
            // does this, we will insert a dummy node to wrap the node for embedding and hold
            // a reference to the dummy node.
            //
            // We will also put a tag on the dummy node so that the Elm developer knows who went
            // behind their back and rudely put stuff in their DOM.
            var dummyNode = document.createElement("div");
            dummyNode.setAttribute("data-elm-hot", "true");
            dummyNode.style.height = "inherit";
            var parentNode = node.parentNode;
            parentNode.replaceChild(dummyNode, node);
            dummyNode.appendChild(node);
            return dummyNode;
        }

        function wrapPublicModule(path, module) {
            var originalInit = module.init;
            if (originalInit) {
                module.init = function (args) {
                    var elm;
                    var portSubscribes = {};
                    var portSends = {};
                    var domNode = null;
                    var flags = null;
                    if (typeof args !== 'undefined') {
                        // normal case
                        domNode = args['node'] && !isFullscreenApp()
                            ? wrapDomNode(args['node'])
                            : document.body;
                        flags = args['flags'];
                    } else {
                        // rare case: Elm allows init to be called without any arguments at all
                        domNode = document.body;
                        flags = undefined
                    }
                    initializingInstance = registerInstance(domNode, flags, path, portSubscribes, portSends);
                    elm = originalInit(args);
                    wrapPorts(elm, portSubscribes, portSends);
                    initializingInstance = null;
                    return elm;
                };
            } else {
                console.error("Could not find a public module to wrap at path " + path)
            }
        }

        function swap(Elm, instance) {
            log('[elm-hot] Hot-swapping module: ' + instance.path);

            swappingInstance = instance;

            // remove from the DOM everything that had been created by the old Elm app
            var containerNode = instance.domNode;
            while (containerNode.lastChild) {
                containerNode.removeChild(containerNode.lastChild);
            }

            var m = getAt(instance.path.split('.'), Elm);
            var elm;
            if (m) {
                // prepare to initialize the new Elm module
                var args = {flags: instance.flags};
                if (containerNode === document.body) {
                    // fullscreen case: no additional args needed
                } else {
                    // embed case: provide a new node for Elm to use
                    var nodeForEmbed = document.createElement("div");
                    containerNode.appendChild(nodeForEmbed);
                    args['node'] = nodeForEmbed;
                }

                elm = m.init(args);

                Object.keys(instance.portSubscribes).forEach(function (portName) {
                    if (portName in elm.ports && 'subscribe' in elm.ports[portName]) {
                        var handlers = instance.portSubscribes[portName];
                        if (!handlers.length) {
                            return;
                        }
                        log('[elm-hot] Reconnect ' + handlers.length + ' handler(s) to port \''
                            + portName + '\' (' + instance.path + ').');
                        handlers.forEach(function (handler) {
                            elm.ports[portName].subscribe(handler);
                        });
                    } else {
                        delete instance.portSubscribes[portName];
                        log('[elm-hot] Port was removed: ' + portName);
                    }
                });

                Object.keys(instance.portSends).forEach(function (portName) {
                    if (portName in elm.ports && 'send' in elm.ports[portName]) {
                        log('[elm-hot] Replace old port send with the new send');
                        instance.portSends[portName] = elm.ports[portName].send;
                    } else {
                        delete instance.portSends[portName];
                        log('[elm-hot] Port was removed: ' + portName);
                    }
                });
            } else {
                log('[elm-hot] Module was removed: ' + instance.path);
            }

            swappingInstance = null;
        }

        function wrapPorts(elm, portSubscribes, portSends) {
            var portNames = Object.keys(elm.ports || {});
            //hook ports
            if (portNames.length) {
                // hook outgoing ports
                portNames
                    .filter(function (name) {
                        return 'subscribe' in elm.ports[name];
                    })
                    .forEach(function (portName) {
                        var port = elm.ports[portName];
                        var subscribe = port.subscribe;
                        var unsubscribe = port.unsubscribe;
                        elm.ports[portName] = Object.assign(port, {
                            subscribe: function (handler) {
                                log('[elm-hot] ports.' + portName + '.subscribe called.');
                                if (!portSubscribes[portName]) {
                                    portSubscribes[portName] = [handler];
                                } else {
                                    //TODO handle subscribing to single handler more than once?
                                    portSubscribes[portName].push(handler);
                                }
                                return subscribe.call(port, handler);
                            },
                            unsubscribe: function (handler) {
                                log('[elm-hot] ports.' + portName + '.unsubscribe called.');
                                var list = portSubscribes[portName];
                                if (list && list.indexOf(handler) !== -1) {
                                    list.splice(list.lastIndexOf(handler), 1);
                                } else {
                                    console.warn('[elm-hot] ports.' + portName + '.unsubscribe: handler not subscribed');
                                }
                                return unsubscribe.call(port, handler);
                            }
                        });
                    });

                // hook incoming ports
                portNames
                    .filter(function (name) {
                        return 'send' in elm.ports[name];
                    })
                    .forEach(function (portName) {
                        var port = elm.ports[portName];
                        portSends[portName] = port.send;
                        elm.ports[portName] = Object.assign(port, {
                            send: function (val) {
                                return portSends[portName].call(port, val);
                            }
                        });
                    });
            }
            return portSubscribes;
        }

        /*
        Breadth-first search for a `Browser.Navigation.Key` in the user's app model.
        Returns the key and keypath or null if not found.
        */
        function findNavKey(rootModel) {
            var queue = [];
            if (isDebuggerModel(rootModel)) {
                /*
                 Extract the user's app model from the Elm Debugger's model. The Elm debugger
                 can hold multiple references to the user's model (e.g. in its "history"). So
                 we must be careful to only search within the "state" part of the Debugger.
                */
                queue.push({value: rootModel['state'], keypath: ['state']});
            } else {
                queue.push({value: rootModel, keypath: []});
            }

            while (queue.length !== 0) {
                var item = queue.shift();

                if (typeof item.value === "undefined" || item.value === null) {
                    continue;
                }

                // The nav key is identified by a runtime tag added by the elm-hot injector.
                if (item.value.hasOwnProperty("elm-hot-nav-key")) {
                    // found it!
                    return item;
                }

                if (typeof item.value !== "object") {
                    continue;
                }

                for (var propName in item.value) {
                    if (!item.value.hasOwnProperty(propName)) continue;
                    var newKeypath = item.keypath.slice();
                    newKeypath.push(propName);
                    queue.push({value: item.value[propName], keypath: newKeypath})
                }
            }

            return null;
        }


        function isDebuggerModel(model) {
            // Up until elm/browser 1.0.2, the Elm debugger could be identified by a
            // property named "expando". But in version 1.0.2 that was renamed to "expandoModel"
            return model
                && (model.hasOwnProperty("expando") || model.hasOwnProperty("expandoModel"))
                && model.hasOwnProperty("state");
        }

        function getAt(keyPath, obj) {
            return keyPath.reduce(function (xs, x) {
                return (xs && xs[x]) ? xs[x] : null
            }, obj)
        }

        function removeNavKeyListeners(navKey) {
            window.removeEventListener('popstate', navKey.value);
            window.navigator.userAgent.indexOf('Trident') < 0 || window.removeEventListener('hashchange', navKey.value);
        }

        // hook program creation
        var initialize = _Platform_initialize;
        _Platform_initialize = function (flagDecoder, args, init, update, subscriptions, stepperBuilder) {
            var instance = initializingInstance || swappingInstance;
            var tryFirstRender = !!swappingInstance;

            var hookedInit = function (args) {
                var initialStateTuple = init(args);
                if (swappingInstance) {
                    var oldModel = swappingInstance.lastState;
                    var newModel = initialStateTuple.a;

                    if (typeof elmSymbol("elm$browser$Browser$application") !== 'undefined') {
                        var oldKeyLoc = findNavKey(oldModel);

                        // attempt to find the Browser.Navigation.Key in the newly-constructed model
                        // and bring it along with the rest of the old data.
                        var newKeyLoc = findNavKey(newModel);
                        var error = null;
                        if (newKeyLoc === null) {
                            error = "could not find Browser.Navigation.Key in the new app model";
                        } else if (oldKeyLoc === null) {
                            error = "could not find Browser.Navigation.Key in the old app model.";
                        } else if (newKeyLoc.keypath.toString() !== oldKeyLoc.keypath.toString()) {
                            error = "the location of the Browser.Navigation.Key in the model has changed.";
                        } else {
                            // remove event listeners attached to the old nav key
                            removeNavKeyListeners(oldKeyLoc.value);

                            // insert the new nav key into the old model in the exact same location
                            var parentKeyPath = oldKeyLoc.keypath.slice(0, -1);
                            var lastSegment = oldKeyLoc.keypath.slice(-1)[0];
                            var oldParent = getAt(parentKeyPath, oldModel);
                            oldParent[lastSegment] = newKeyLoc.value;
                        }

                        if (error !== null) {
                            console.error("[elm-hot] Hot-swapping " + instance.path + " not possible: " + error);
                            oldModel = newModel;
                        }
                    }

                    // the heart of the app state hot-swap
                    initialStateTuple.a = oldModel;

                    // ignore any Cmds returned by the init during hot-swap
                    initialStateTuple.b = elmSymbol("elm$core$Platform$Cmd$none");
                } else {
                    // capture the initial state for later
                    initializingInstance.lastState = initialStateTuple.a;
                }

                return initialStateTuple
            };

            var hookedStepperBuilder = function (sendToApp, model) {
                var result;
                // first render may fail if shape of model changed too much
                if (tryFirstRender) {
                    tryFirstRender = false;
                    try {
                        result = stepperBuilder(sendToApp, model)
                    } catch (e) {
                        throw new Error('[elm-hot] Hot-swapping ' + instance.path +
                            ' is not possible, please reload page. Error: ' + e.message)
                    }
                } else {
                    result = stepperBuilder(sendToApp, model)
                }

                return function (nextModel, isSync) {
                    if (instance) {
                        // capture the state after every step so that later we can restore from it during a hot-swap
                        instance.lastState = nextModel
                    }
                    return result(nextModel, isSync)
                }
            };

            return initialize(flagDecoder, args, hookedInit, update, subscriptions, hookedStepperBuilder)
        };

        // hook process creation
        var originalBinding = _Scheduler_binding;
        _Scheduler_binding = function (originalCallback) {
            return originalBinding(function () {
                // start the scheduled process, which may return a cancellation function.
                var cancel = originalCallback.apply(this, arguments);
                if (cancel) {
                    cancellers.push(cancel);
                    return function () {
                        cancellers.splice(cancellers.indexOf(cancel), 1);
                        return cancel();
                    };
                }
                return cancel;
            });
        };

        scope['_elm_hot_loader_init'] = function (Elm) {
            // swap instances
            var removedInstances = [];
            for (var id in instances) {
                var instance = instances[id];
                if (instance.domNode.parentNode) {
                    swap(Elm, instance);
                } else {
                    removedInstances.push(id);
                }
            }

            removedInstances.forEach(function (id) {
                delete instance[id];
            });

            // wrap all public modules
            var publicModules = findPublicModules(Elm);
            publicModules.forEach(function (m) {
                wrapPublicModule(m.path, m.module);
            });
        }
    })();

    scope['_elm_hot_loader_init'](scope['Elm']);
}
//////////////////// HMR END ////////////////////


}(this));
},{}],"src/main.js":[function(require,module,exports) {
require('./style.css');

var _require = require('./Main.elm'),
    Elm = _require.Elm;

var app = Elm.Main.init({
  node: document.getElementById('elm')
});
app.ports.saveUser.subscribe(function (data) {
  localStorage.setItem("User", JSON.stringify(data));
});
app.ports.getUser.subscribe(function () {
  var data = JSON.parse(localStorage.getItem("User"));
  app.ports.restoreUser.send(data);
});
},{"./style.css":"src/style.css","./Main.elm":"src/Main.elm"}],"C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "50314" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else if (location.reload) {
        // `location` global exists in a web worker context but lacks `.reload()` function.
        location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["C:/Users/a/AppData/Roaming/npm/node_modules/parcel/src/builtins/hmr-runtime.js","src/main.js"], null)
//# sourceMappingURL=/main.1e43358e.js.map