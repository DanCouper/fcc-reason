// Generated by BUCKLESCRIPT VERSION 1.7.1, PLEASE EDIT WITH CARE
'use strict';

import * as Char        from "bs-platform/lib/es6/char.js";
import * as List        from "bs-platform/lib/es6/list.js";
import * as $$String    from "bs-platform/lib/es6/string.js";
import * as Caml_string from "bs-platform/lib/es6/caml_string.js";

function explode(str) {
  var _a = str.length - 1 | 0;
  var _b = /* [] */0;
  while(true) {
    var b = _b;
    var a = _a;
    if (a < 0) {
      return b;
    } else {
      _b = /* :: */[
        Caml_string.get(str, a),
        b
      ];
      _a = a - 1 | 0;
      continue ;
      
    }
  };
}

function uppercaseAsciiRot(rotN, str) {
  return $$String.concat("", List.map(function (c) {
                  return $$String.make(1, c);
                }, List.fold_right(function ($$char, acc) {
                      if ($$char >= 65 && $$char <= 90) {
                        var ccode = $$char + rotN | 0;
                        if (ccode > 90) {
                          return /* :: */[
                                  Char.chr(154 - ccode | 0),
                                  acc
                                ];
                        } else {
                          return /* :: */[
                                  Char.chr(ccode),
                                  acc
                                ];
                        }
                      } else {
                        return /* :: */[
                                $$char,
                                acc
                              ];
                      }
                    }, explode(str), /* [] */0)));
}

function decode(str) {
  return uppercaseAsciiRot(13, str);
}

export {
  explode           ,
  uppercaseAsciiRot ,
  decode            ,
  
}
/* No side effect */