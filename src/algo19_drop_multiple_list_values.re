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

let destroyer arr targets => arr -@ targets;
