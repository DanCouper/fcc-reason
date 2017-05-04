// Generated by BUCKLESCRIPT VERSION 1.7.1, PLEASE EDIT WITH CARE
'use strict';

var List  = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");

function nativeFind(list, pred) {
  return List.find(pred, list);
}

function recursiveFind(list, pred) {
  var _l = list;
  while(true) {
    var l = _l;
    if (l) {
      var hd = l[0];
      if (Curry._1(pred, hd)) {
        return hd;
      } else {
        _l = l[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

exports.nativeFind    = nativeFind;
exports.recursiveFind = recursiveFind;
/* No side effect */