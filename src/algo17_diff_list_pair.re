/**
 * Compare two arrays and return a new array with any items only found
 * in one of the two given arrays, but not both. In other words, return
 * the symmetric difference of the two arrays.
 */
let pushIfAbsent xs x =>
  if (List.mem x xs) {
    xs
  } else {
    [x, ...xs]
  };

let unique xs => List.rev (List.fold_left pushIfAbsent [] xs);

let difference list1 list2 => unique (List.filter (fun x => not (List.mem x list2)) list1);

let symmetricDifference list1 list2 => difference list2 list1 @ difference list1 list2;


/**
 * NOTE using functions ported from `Containers` library;
 * (see [https://github.com/c-cube/ocaml-containers]).
 *
 * Then defining infix functions for diff & symmetric diff.
 */
let uniq ::eq=(==) l => {
  let rec uniq eq acc l =>
    switch l {
    | [] => List.rev acc
    | [x, ...xs] when List.exists (eq x) xs => uniq eq acc xs
    | [x, ...xs] => uniq eq [x, ...acc] xs
    };
  uniq eq [] l
};

let (-@) a b => uniq (List.filter (fun v => not (List.mem v b)) a);

let (-@-) a b => a -@ b @ b -@ a;
